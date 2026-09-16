(async function () {
  const model = await fetch('modelo.json').then(r => {
    if (!r.ok) throw new Error('No se pudo cargar modelo.json');
    return r.json();
  });

  const coef = Object.fromEntries(model.coeficientes.map(d => [d.term, d.estimate]));
  const ref = model.perfil_referencia[0];
  const ranges = Object.fromEntries(model.rangos.map(d => [d.variable, d]));

  const els = {
    escs: document.querySelector('#escs'),
    selfreg: document.querySelector('#selfreg'),
    ampliacion: document.querySelector('#ampliacion'),
    sustitucion: document.querySelector('#sustitucion'),
    escsOut: document.querySelector('#escsOut'),
    selfregOut: document.querySelector('#selfregOut'),
    ampliacionOut: document.querySelector('#ampliacionOut'),
    sustitucionOut: document.querySelector('#sustitucionOut'),
    score: document.querySelector('#score'),
    comparison: document.querySelector('#comparison'),
    explain: document.querySelector('#explain'),
    meterFill: document.querySelector('#meterFill'),
    reset: document.querySelector('#reset')
  };

  function setRange(el, r) {
    el.min = r.p05;
    el.max = r.p95;
  }
  setRange(els.escs, ranges.ESCS);
  setRange(els.selfreg, ranges.SELFREG);

  function radio(name) {
    return document.querySelector(`input[name="${name}"]:checked`).value;
  }
  function setRadio(name, value) {
    const el = document.querySelector(`input[name="${name}"][value="${value}"]`);
    if (el) el.checked = true;
  }

  function reset() {
    setRadio('sexo', ref.sexo);
    setRadio('repeat', ref.REPEAT);
    els.escs.value = ref.ESCS;
    els.selfreg.value = ref.SELFREG;
    els.ampliacion.value = Math.round(ref.ia_ampliacion * 2) / 2;
    els.sustitucion.value = Math.round(ref.ia_sustitucion * 2) / 2;
    update();
  }

  function predict(profile) {
    let y = coef['(Intercept)'];
    y += coef.ESCS * profile.ESCS;
    y += coef.BELONG * ref.BELONG; // ajuste oculto, fijado en la media
    y += coef.SELFREG * profile.SELFREG;
    y += coef.ia_ampliacion * profile.ia_ampliacion;
    y += coef.ia_sustitucion * profile.ia_sustitucion;
    if (profile.sexo === 'Mujer') y += coef.sexoMujer;
    if (profile.REPEAT === 'Sí') y += coef['REPEATSí'];
    return y;
  }

  const referenceScore = predict({
    sexo: ref.sexo,
    REPEAT: ref.REPEAT,
    ESCS: ref.ESCS,
    SELFREG: ref.SELFREG,
    ia_ampliacion: ref.ia_ampliacion,
    ia_sustitucion: ref.ia_sustitucion
  });

  function update() {
    const p = {
      sexo: radio('sexo'),
      REPEAT: radio('repeat'),
      ESCS: +els.escs.value,
      SELFREG: +els.selfreg.value,
      ia_ampliacion: +els.ampliacion.value,
      ia_sustitucion: +els.sustitucion.value
    };
    const y = predict(p);
    const delta = y - referenceScore;

    els.escsOut.textContent = p.ESCS.toFixed(2);
    els.selfregOut.textContent = p.SELFREG.toFixed(2);
    els.ampliacionOut.textContent = p.ia_ampliacion.toFixed(1);
    els.sustitucionOut.textContent = p.ia_sustitucion.toFixed(1);
    els.score.textContent = Math.round(y);
    els.meterFill.style.width = `${Math.max(0, Math.min(100, (y - 250) / 4))}%`;

    if (Math.abs(delta) < .5) {
      els.comparison.textContent = 'Este perfil coincide aproximadamente con el perfil de referencia del modelo.';
    } else {
      els.comparison.textContent = `${Math.abs(delta).toFixed(0)} puntos ${delta > 0 ? 'por encima' : 'por debajo'} del perfil de referencia.`;
    }

    els.explain.textContent = 'La pertenencia escolar permanece en el modelo como variable de ajuste, fijada en su valor promedio, pero no se muestra como control.';
  }

  document.querySelectorAll('input').forEach(el => el.addEventListener('input', update));
  els.reset.addEventListener('click', reset);
  reset();
})().catch(err => {
  document.querySelector('#score').textContent = 'Error';
  document.querySelector('#comparison').textContent = err.message;
});
