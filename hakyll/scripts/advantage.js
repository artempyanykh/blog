const plotAbsCanvas = document.getElementById('plot1');

const calcProbAdvantage = function (p) {
  return 1 - Math.pow(1 - p, 2);
}

const calcPropDisadvantage = function (p) {
  return Math.pow(p, 2);
}

const resolution = 20;
const dieRolls = Array.from(Array(resolution + 1).keys());
const probs = dieRolls.map(x => x / resolution);
const probsAdvantage = probs.map(calcProbAdvantage);
const probsDisadvantage = probs.map(calcPropDisadvantage);

const labels = probs.map(x => (x * 100).toFixed(0) + '%');

const plotAbs = new Chart(plotAbsCanvas, {
  type: 'line',
  data: {
    labels: labels, // x-axis labels should be the possible die rolls
    datasets: [
      {
        label: 'Normal',
        data: probs,
        borderColor: 'rgba(0, 123, 255, 1)', // blue
        fill: false
      },
      {
        label: 'Advantage',
        data: probsAdvantage,
        borderColor: 'rgba(40, 167, 69, 1)', // green
        fill: false
      },
      {
        label: 'Disadvantage',
        data: probsDisadvantage,
        borderColor: 'rgba(220, 53, 69, 1)', // red
        fill: false
      }
    ]
  },
  options: {
    scales: {
      y: {
        beginAtZero: true
      }
    }
  }
});

var plotUpliftCanvas = document.getElementById('plot2');

const eps = 1e-10;
const divCheck0 = function (x, y) { return Math.abs(y) < eps ? 0 : x / y; }
const advantageUplift = probsAdvantage.map((pa, i) => divCheck0(pa, probs[i]));
const disadvantageDrop = probsDisadvantage.map((pd, i) => divCheck0(probs[i], pd));

const plotUplift = new Chart(plotUpliftCanvas, {
  type: 'line',
  data: {
    labels: labels, // x-axis labels should be the possible die rolls
    datasets: [
      {
        label: 'Advantage Uplift',
        data: advantageUplift,
        borderColor: 'rgba(40, 167, 69, 1)', // green
        fill: false
      }
    ]
  },
  options: {
    scales: {
      y: {
        beginAtZero: true
      }
    }
  }
});

var plotDropCanvas = document.getElementById('plot3');
const plotDrop = new Chart(plotDropCanvas, {
  type: 'line',
  data: {
    labels: labels, // x-axis labels should be the possible die rolls
    datasets: [
      {
        label: 'Disadvantage Drop',
        data: disadvantageDrop,
        borderColor: 'rgba(220, 53, 69, 1)', // red
        fill: false
      }
    ]
  },
  options: {
    scales: {
      y: {
        beginAtZero: true
      }
    }
  }
});
