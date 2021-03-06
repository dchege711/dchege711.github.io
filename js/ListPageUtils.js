function toggleSummaryDisplay(button) {
    // We adopted the convention that the button with ID
    // `<hash>-summary-controller` controls the summary paragraph with ID
    // `<hash>-summary`.
    let summaryIDIdx = button.id.indexOf("-controller");
    if (summaryIDIdx === -1) return;

    let summaryParagraph = document.getElementById(button.id.substring(0, summaryIDIdx));
    if (summaryParagraph.style.display === "none") {
        summaryParagraph.style.display = "inherit";
        button.innerHTML = `<i class="fas fa-chevron-up"></i>`;
    } else if (summaryParagraph.style.display === "inherit") {
        summaryParagraph.style.display = "none";
        button.innerHTML = `<i class="fas fa-chevron-down"></i>`;
    }
}
