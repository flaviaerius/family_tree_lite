// Faz as bolas, as fotos e os nomes da árvore acompanharem o zoom e a largura
// da janela.
//
// Por que em JS: no plotly, `marker.size` e `textfont.size` são em pixels e não
// reagem nem ao zoom nem ao resize, enquanto o espaçamento entre as pessoas é
// medido em unidades de dados. O resultado era uma bola de 22px fixos com um gap
// entre irmãos que ia de 10px (janela estreita, bolas sobrepostas) a 27px
// (janela larga), e uma foto — essa sim desenhada em unidades de dados — que só
// preenchia o anel em telas muito largas.
//
// A correção inverte a fonte da verdade: o diâmetro passa a ser definido em
// unidades de dados e os pixels são derivados dele a cada redesenho. Fazer isso
// no servidor via plotly_relayout exigiria re-renderizar a cada scroll e perder
// o estado do zoom; o restyle daqui é instantâneo.

(function () {
  var PLOT_ID = "csv_plot";

  var NODE_DIAMETER_UNITS = 0.9; // diâmetro da bola, em unidades do eixo x
  var MIN_BALL_PX = 6;
  var MAX_BALL_PX = 46;
  var FONT_RATIO = 0.75; // fonte como fração do diâmetro da bola
  var MIN_FONT_PX = 5;
  var MAX_FONT_PX = 16; // espelha LABEL_MAX_FONT_PX em R/plot_overview.R
  var HIDE_LABELS_BELOW_PX = 5.5; // abaixo disso o nome vira borrão: esconde
  var FALLBACK_FIRST_NAME_PX_PER_UNIT = 50; // se o layout não trouxer o cálculo

  function clamp(v, lo, hi) {
    return Math.max(lo, Math.min(hi, v));
  }


  // Os traces de pessoas desenham marcadores; os de ligação são só linhas.
  function nodeTraces(gd) {
    var idx = [];
    (gd.data || []).forEach(function (t, i) {
      if (t.mode && t.mode.indexOf("markers") >= 0) idx.push(i);
    });
    return idx;
  }

  function rescale(gd) {
    var fl = gd._fullLayout;
    if (!fl || !fl.xaxis || !fl.xaxis.range) return;

    var spanX = Math.abs(fl.xaxis.range[1] - fl.xaxis.range[0]);
    var widthPx = fl.xaxis._length;
    if (!spanX || !widthPx) return;
    var pxPerUnitX = widthPx / spanX;

    var ball = clamp(NODE_DIAMETER_UNITS * pxPerUnitX, MIN_BALL_PX, MAX_BALL_PX);
    var font = clamp(ball * FONT_RATIO, MIN_FONT_PX, MAX_FONT_PX);
    var showLabels = ball * FONT_RATIO >= HIDE_LABELS_BELOW_PX;

    // Todo mundo mostra o primeiro nome, em qualquer zoom (labels_default e
    // labels_zoomed vêm iguais do R); só a geração mais antiga de cada núcleo
    // leva letra maior, via annotations[j].font.size mais abaixo.
    var meta = (gd.layout && gd.layout.meta) || {};
    var firstNameFrom =
      meta.first_name_px_per_unit || FALLBACK_FIRST_NAME_PX_PER_UNIT;
    var showFullName = pxPerUnitX >= firstNameFrom;

    // Dois controles separados. Sem isso o relayout das fotos reentraria neste
    // mesmo código em laço; e um controle só faria a foto parar de acompanhar
    // assim que a bola batesse no teto — no zoom forte ela cresceria para fora
    // do anel, que é justamente o descolamento que queremos eliminar.
    var idx = nodeTraces(gd);
    if (!idx.length) return;

    var styleState = ball.toFixed(2);
    if (gd._treeScaleStyle !== styleState) {
      gd._treeScaleStyle = styleState;
      Plotly.restyle(gd, { "marker.size": ball }, idx);
    }

    // A foto tem que virar os mesmos pixels da bola nos dois eixos. Como o eixo
    // y está preso ao x por um scaleratio, um "1" vertical não vale um "1"
    // horizontal — daí derivar cada lado do seu próprio px/unidade.
    var spanY = Math.abs(fl.yaxis.range[1] - fl.yaxis.range[0]);
    var pxPerUnitY = spanY ? fl.yaxis._length / spanY : pxPerUnitX;
    var sizeX = ball / pxPerUnitX;
    var sizeY = ball / pxPerUnitY;

    // Os nomes são anotações (precisam de fundo opaco para as linhas de ligação
    // não os cortarem), então quem escala a fonte é o relayout, não o restyle.
    var imgs = (gd.layout && gd.layout.images) || [];
    var anns = (gd.layout && gd.layout.annotations) || [];
    if (!imgs.length && !anns.length) return;

    var layoutState =
      sizeX.toFixed(4) + "|" + sizeY.toFixed(4) + "|" +
      font.toFixed(2) + "|" + showLabels + "|" + showFullName;
    if (gd._treeScaleLayout === layoutState) return;
    gd._treeScaleLayout = layoutState;

    var texts = showFullName ? meta.labels_zoomed : meta.labels_default;
    var scale = meta.labels_scale || [];

    var upd = {};
    for (var i = 0; i < imgs.length; i++) {
      upd["images[" + i + "].sizex"] = sizeX;
      upd["images[" + i + "].sizey"] = sizeY;
    }
    for (var j = 0; j < anns.length; j++) {
      // A geração mais antiga leva letra maior, por um multiplicador que o R
      // manda pessoa a pessoa.
      upd["annotations[" + j + "].font.size"] = font * (scale[j] || 1);
      upd["annotations[" + j + "].visible"] = showLabels;
      if (texts && texts[j] !== undefined) {
        upd["annotations[" + j + "].text"] = texts[j];
      }
    }
    Plotly.relayout(gd, upd);
  }

  function attach(gd) {
    if (gd._treeScaleAttached) return;
    gd._treeScaleAttached = true;
    gd.on("plotly_afterplot", function () {
      rescale(gd);
    });
    gd.on("plotly_relayout", function () {
      rescale(gd);
    });
    rescale(gd);
  }

  function scan(tries) {
    var gd = document.getElementById(PLOT_ID);
    if (gd && typeof gd.on === "function" && gd._fullLayout) {
      attach(gd);
      return;
    }
    if ((tries || 0) < 50) {
      setTimeout(function () {
        scan((tries || 0) + 1);
      }, 100);
    }
  }

  document.addEventListener("shiny:value", function (e) {
    if (e.target && e.target.id === PLOT_ID) setTimeout(scan, 0);
  });

  window.addEventListener("resize", function () {
    var gd = document.getElementById(PLOT_ID);
    if (gd && gd._fullLayout) {
      gd._treeScaleStyle = null; // a largura mudou: recalcula
      gd._treeScaleLayout = null;
      setTimeout(function () {
        rescale(gd);
      }, 150);
    }
  });

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () {
      scan(0);
    });
  } else {
    scan(0);
  }
})();
