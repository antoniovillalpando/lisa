(async function () {
  const model = await fetch('modelo.json').then(r => {
    if (!r.ok) throw new Error('No se pudo cargar modelo.json');
    return r.json();
  });

  const ref = model.perfil_referencia[0];
  const ranges = Object.fromEntries(model.rangos.map(d => [d.variable, d]));
  let activeDomain = 'Matematicas';

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
    meterFill: document.querySelector('#meterFill'),
    reset: document.querySelector('#reset'),
    resultDomain: document.querySelector('#resultDomain'),
    method: document.querySelector('#method'),
    glossary: document.querySelector('#glossary')
  };

  function coefficients(domain) {
    return Object.fromEntries(
      model.dominios[domain].coeficientes.map(d => [d.term, d.estimate])
    );
  }

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

  function profileFromControls() {
    return {
      sexo: radio('sexo'),
      REPEAT: radio('repeat'),
      ESCS: +els.escs.value,
      SELFREG: +els.selfreg.value,
      ia_ampliacion: +els.ampliacion.value,
      ia_sustitucion: +els.sustitucion.value
    };
  }

  function predict(profile, domain = activeDomain) {
    const coef = coefficients(domain);
    let y = coef['(Intercept)'];
    y += coef.ESCS * profile.ESCS;
    y += coef.BELONG * ref.BELONG;
    y += coef.SELFREG * profile.SELFREG;
    y += coef.ia_ampliacion * profile.ia_ampliacion;
    y += coef.ia_sustitucion * profile.ia_sustitucion;
    if (profile.sexo === 'Mujer') y += coef.sexoMujer;
    if (profile.REPEAT === 'Sí') y += coef['REPEATSí'];
    return y;
  }

  function referenceScore(domain = activeDomain) {
    return predict({
      sexo: ref.sexo,
      REPEAT: ref.REPEAT,
      ESCS: ref.ESCS,
      SELFREG: ref.SELFREG,
      ia_ampliacion: ref.ia_ampliacion,
      ia_sustitucion: ref.ia_sustitucion
    }, domain);
  }

  function update() {
    const p = profileFromControls();
    const y = predict(p);
    const delta = y - referenceScore();
    const domainLabel = model.dominios[activeDomain].dominio;

    els.escsOut.textContent = p.ESCS.toFixed(2);
    els.selfregOut.textContent = p.SELFREG.toFixed(2);
    els.ampliacionOut.textContent = p.ia_ampliacion.toFixed(1);
    els.sustitucionOut.textContent = p.ia_sustitucion.toFixed(1);
    els.score.textContent = Math.round(y);
    els.resultDomain.textContent = domainLabel.toUpperCase();
    els.meterFill.style.width = `${Math.max(0, Math.min(100, (y - 250) / 4))}%`;

    if (Math.abs(delta) < .5) {
      els.comparison.textContent = 'Este perfil coincide aproximadamente con el promedio nacional de referencia.';
    } else {
      els.comparison.textContent = `${Math.abs(delta).toFixed(0)} puntos ${delta > 0 ? 'por encima' : 'por debajo'} del perfil de referencia en ${domainLabel.toLowerCase()}.`;
    }
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

  function renderGlossary() {
    const visible = new Set(['ESCS', 'REPEAT', 'SELFREG', 'ia_ampliacion', 'ia_sustitucion']);
    els.glossary.innerHTML = model.glosario
      .filter(d => visible.has(d.variable))
      .map(d => `<article class="glossary-item"><h3>${d.etiqueta}</h3><p>${d.definicion}</p></article>`)
      .join('');
  }

  document.querySelectorAll('input').forEach(el => el.addEventListener('input', update));
  els.reset.addEventListener('click', reset);

  document.querySelectorAll('.domain').forEach(button => {
    button.addEventListener('click', () => {
      activeDomain = button.dataset.domain;
      document.querySelectorAll('.domain').forEach(b => b.classList.toggle('active', b === button));
      update();
    });
  });

  els.method.textContent = model.nota;
  renderGlossary();
  reset();
})().catch(err => {
  document.querySelector('#score').textContent = 'Error';
  document.querySelector('#comparison').textContent = err.message;
});
