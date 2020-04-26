/**
 * This file assumes that both `static/js/PlotUtils.js` and
 * `static/js/d3/d3.min.js` are in scope of the current document.
 */

/**
 * Draw the SI model.
 */

let paramsSIModel = {
    infectionZero: 0.3,
    beta: 0.01,
    maxTime: 100,
}

let configSIModel = {
    holderElementId: "si-model-plot",
    params: [
        {
            type: "range", min: 0, max: 1, id: "beta",
            label: "\\(\\beta\\): fraction of infected people that infect others per time unit",
            update: (event) => {
                paramsSIModel.beta = event.target.value;
                updateSIModel();
            },
            value: () => { return paramsSIModel.beta; }
        },
        {
            type: "range", min: 0, max: 1, id: "infection-zero",
            label: "\\(I(0)\\): fraction of people infected at \\(t = 0\\)",
            update: (event) => {
                paramsSIModel.infectionZero = event.target.value;
                updateSIModel();
            },
            value: () => { return paramsSIModel.infectionZero; }
        },
    ],
    x: {
        data: d3.range(0, paramsSIModel.maxTime, 1),
        label: "Time (t)"
    },
    y: {
        label: "Proportion"
    },
    lineSpecs: [
        {
            f: (t) => {
                let susceptibleZero = 1 - paramsSIModel.infectionZero;
                let eToBetaTime = Math.exp(paramsSIModel.beta * t);
                return paramsSIModel.infectionZero * eToBetaTime * 1.0 / (susceptibleZero + paramsSIModel.infectionZero * eToBetaTime);
            },
            label: "I(t)",
            color: "red"
        },
        {
            f: (t) => {
                let susceptibleZero = 1 - paramsSIModel.infectionZero;
                let eToBetaTime = Math.exp(paramsSIModel.beta * t);
                return susceptibleZero / (susceptibleZero + paramsSIModel.infectionZero * eToBetaTime);
            },
            label: "S(t)",
            color: "blue"
        }
    ]
}

function updateSIModel() {
    drawInteractiveLinearChart(configSIModel);
}
updateSIModel();

    
