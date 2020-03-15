/**
 * A helper for drawing line charts using D3.js. Adapted from [1] and [2]. This
 * function assumes that the appropriate D3.js libraries have been loaded in the
 * scope of the page (i.e. there is a <script> tag pointing to D3.js
 * dependencies).
 * 
 * @param {Object} config Everything needed to draw the plots should be provided
 * through the config object. Expected properties include:
 * 
 * - `holderElementId`: The ID of the element that the chart will be a direct
 *   child of.
 * 
 * - `x`: An object describing the values that will appear on the x axis. The
 *   properties should include: `data` - an array of numerical values; `label` -
 *   the string that should be used to label the x-axis.
 * 
 * - `y`: An object describing how the y-axis will be presented. Properties
 *   should include: `label` - a string for labelling the y-axis.
 * 
 * - `lineSpecs`: An array of objects, each of which specify how a line should
 *   be drawn. Each object should have the following properties: `f`, `label`.
 *   `label`.
 *   `f` is a function that computes `y = f(x)`, as defined in math notation.
 *   `label` is a string used to label the resulting plot.
 *   `color` is a string denoting the color of the resulting plot.
 * 
 * [1]: https://observablehq.com/@d3/line-chart
 * 
 * [2]: https://bl.ocks.org/d3noob/402dd382a51a4f6eea487f9a35566de0
 */
function drawInteractiveLinearChart(config) {

    // Set dimensions and margins of the graph
    let parentElem = document.getElementById(config.holderElementId);
    const width = parentElem.scrollWidth * 0.8;
    const height = width; // Because scrollWidth is zero at the beginning
    const margin = { top: 20, right: 30, bottom: 30, left: 40 };

    let data = config.x.data;

    /**
     * More information about working with continuous values:
     * https://github.com/d3/d3-scale/blob/v2.2.2/README.md#continuous-scales
     */

    // Set the configuration for the X axis.
    let x = d3.scaleLinear()
        // `domain` refers to the possible values that x can take.
        .domain([data[0], data[data.length - 1]])
        // `range` is used to map a point to SVG coordinates
        .range([margin.left, width - margin.right]);

    // TODO: How do we set the domain for the y-axis without calculating it
    // first?
    let y = d3.scaleLinear()
        .range([height - margin.bottom, margin.top]);

    let yAxis = g => g
        .attr("transform", `translate(${margin.left}, 0)`)
        .call(d3.axisLeft(y))
        .call(g => g.select(".domain").remove())
        .call(g => g.select(".tick:last-of-type text").clone()
                .attr("x", 3)
                .attr("font-weight", "bold")
                .attr("text-anchor", "start")
                .text(config.y.label));

    let xAxis = g => g
        .attr("transform", `translate(0, ${height - margin.bottom})`)
        .call(d3.axisBottom(x))
        .call(g => g.select(".tick:last-of-type text").clone()
                .attr("y", -10)
                .attr("text-anchor", "start")
                .text(config.x.label));

    // Replace the old SVG element if any.
    let oldSVG = document.getElementById(`${config.holderElementId}-svg`);
    let svg = d3.create("svg");
    let newSVGNode = svg.node();
    if (oldSVG) {
        oldSVG.replaceWith(newSVGNode);
    } else {
        let firstChild = parentElem.firstChild;
        if (firstChild) {
            parentElem.insertBefore(newSVGNode, firstChild);
        } else {
            parentElem.appendChild(newSVGNode);
        }
    }

    svg.attr("id", `${config.holderElementId}-svg`);
    svg.attr("viewBox", [0, 0, width, height]);
    svg.append("g").call(xAxis);
    svg.append("g").call(yAxis);

    config.lineSpecs.forEach((lineSpec) => {
        let yLine = d3.line()
            .defined((t) => { return !isNaN(t); })
            .x(t => x(t) )
            .y(t => y(lineSpec.f(t)));

        // Add the value line path
        svg.append("path")
            .datum(data)
            .attr("fill", "none")
            .attr("stroke", lineSpec.color)
            .attr("d", yLine);

        let lastEntry = data[data.length - 1];
        let lastPoint = {
            x: x(lastEntry),
            y: y(lineSpec.f(lastEntry))
        };

        svg.append("text")
            .attr("transform", `translate(${lastPoint.x}, ${lastPoint.y})`)
            .attr("text-anchor", "start")
            .style("fill", lineSpec.color)
            .style("font-size", "smaller")
            .style("font-weight", "lighter")
            .text(lineSpec.label);
    });

    // If the interactive pane hasn't yet been drawn, draw it
    let interactivePaneId = `${config.holderElementId}-controls`;
    let interactivePane = document.getElementById(interactivePaneId);
    if (interactivePane || config.params.length === 0) return;

    interactivePane = document.createElement("div");
    interactivePane.style.display = "flex";
    interactivePane.style.flexDirection = "column";
    interactivePane.style.justifyContent = "space-between";
    interactivePane.style.alignContent = "space-between";

    interactivePane.id = interactivePaneId;

    config.params.forEach((paramObj) => {
        let paramScopedId = `${interactivePaneId}-${paramObj.id}`;

        let inputElement = document.createElement("input");
        inputElement.type = paramObj.type;
        inputElement.id = paramScopedId;
        inputElement.name = paramScopedId;
        inputElement.min = paramObj.min;
        inputElement.max = paramObj.max;
        inputElement.value = paramObj.value();
        inputElement.step = "any";
        inputElement.addEventListener('change', paramObj.update);

        let inputValueAsText = document.createElement("span");
        inputValueAsText.innerText = paramObj.value();
        inputValueAsText.id = `${paramScopedId}-value`;
        inputElement.addEventListener("change", (event) => {
            let val = event.target.value;
            if (!Number.isNaN(val) && Math.floor(val) !== val) {
                val = Number.parseFloat(val).toFixed(2);
            }
            inputValueAsText.innerText = val;
        });

        let labelElement = document.createElement("label");
        labelElement.for = paramScopedId;
        labelElement.innerText = paramObj.label;

        let container = document.createElement("div");
        container.appendChild(labelElement);
        container.appendChild(document.createElement("br"));
        container.appendChild(inputElement);
        container.appendChild(inputValueAsText);
        
        interactivePane.appendChild(container);
    });

    parentElem.appendChild(interactivePane);

}
