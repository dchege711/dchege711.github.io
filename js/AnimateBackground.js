window.addEventListener("load", updateBodyElement);

async function updateBodyElement() {
    const bodyElem = document.getElementsByTagName("body")[0];
    const svgWidth = bodyElem.scrollWidth;
    const svgHeight = bodyElem.scrollHeight;
    const maxRadius = 60;

    const numCircles = Math.min(
        100,
        Math.floor((svgWidth * svgHeight) / (maxRadius * maxRadius * 8)));

    const svg = d3.select("#background-svg");

    for (let i = 0; i < numCircles; ++i) {
        svg.append("circle")
            .attr("cx", Math.random() * svgWidth)
            .attr("cy", Math.random() * svgHeight)
            .attr("r", Math.random() * maxRadius)
            .attr("fill", "transparent")
            .attr("stroke-width", 1)
            .attr("stroke", "#18b3b3");
    }

    // This animation causes ~20MB spike in memory usage. Disable it.
    // while (true) {
    //     await svg.selectAll("circle")
    //         .transition()
    //         .ease(d3.easeSinInOut)
    //         .duration(10000)
    //         .attr("cx", () => { return Math.random() * svgWidth; })
    //         .attr("cy", () => { return Math.random() * svgHeight; })
    //         .attr("r", () => { return Math.random() * maxRadius; })
    //         .end();
    // }
}
