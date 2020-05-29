const INDEXABLE_FIELDS = ["body", "tags", "origins", "title"];

const DATE_FORMATTER = Intl.DateTimeFormat(
  "default", { year: "numeric", month: "long", day: "numeric" });

/**
 * Assumes that LunrJS is in scope.
 * Source: https://lunrjs.com/guides/getting_started.html
 * Source: https://lunrjs.com/docs/lunr.Builder.html
 */
var idx = lunr(function () {
  // The field that should be used as the document reference. Defaults to `id`.
  this.ref("position_in_index");

  // Document fields that will be indexed.
  INDEXABLE_FIELDS.forEach((indexedField) => { this.field(indexedField); })

  // The content is scrubbed and tokenized, so highlighting the matching part
  // won't do any good.
  this.metadataWhitelist = ["position"];

  searchableContent.forEach(function (doc) {
    this.add(doc);
  }, this);
});

/**
 * Process the search query and display results if appropriate.
 * 
 * @param {Event} event 
 */
function queryProcessor(event) {
  // Ignore all keyup events that are part of composition.
  // https://developer.mozilla.org/en-US/docs/Web/API/Document/keyup_event
  if (event.isComposing || event.keyCode === 229) return;

  // `event.code === "Enter"` does not work on mobile. 13 should work on both.
  // https://stackoverflow.com/a/16317389
  if (event.keyCode === 13) {
    event.preventDefault();
    displaySearchResults(event.srcElement.innerText);
  }
}

window.onload = () => {
  const searchQueryElement = document.getElementById("searchQuery");
  searchQueryElement.addEventListener("keydown", queryProcessor);

  const searchParams = new URLSearchParams(window.location.search);
  if (searchParams.has("q")) {
    searchQueryElement.innerText = searchParams.get("q");
    displaySearchResults(searchParams.get("q"));
  }
}

/**
 * Display the search results that match `query`.
 * 
 * @param {String} query The query string to pass to LunrJs
 */
function displaySearchResults(query) {
  let trimmedQuery = query.trim();
  if (trimmedQuery.length === 0) return;

  let startTime = Date.now();
  let searchResults = idx.search(trimmedQuery);

  let searchResultsHTML = "";
  for (let i = 0; i < searchResults.length; i++) {
    let res = searchResults[i];
    let doc = searchableContent[res.ref];

    let highlightingGuide = {}
    INDEXABLE_FIELDS.forEach((field) => { highlightingGuide[field] = []; });

    let metadataKeys = Object.keys(res.matchData.metadata);
    metadataKeys.forEach((key) => {
      let obj = res.matchData.metadata[key]
      let fields = Object.keys(obj);
      fields.forEach((field) => {
        obj[field].position.forEach((matchPos) => {
          highlightingGuide[field].push(matchPos); 
        });
      });
    });

    searchResultsHTML += createAnnotatedSearchResult(res.ref, highlightingGuide);
  }

  let searchMetadataHTML = `<p class="meta">
    ${searchResults.length} results in 
    ${(Date.now() - startTime) / 1000} seconds. 
    Powered by <a href="https://lunrjs.com/">Lunr JS</a></p>`;

  document.getElementById("searchResults").innerHTML = 
    `${searchMetadataHTML}${searchResultsHTML}`;
  document.title = `${query} | c13u Search`;
}

/**
 * @returns {String} The HTML representation of `matchInfo` superimposed on
 * the matching document. Produces nice snippets.
 *  
 * @param {Number} docIdx The index of the matching document.
 * @param {Object} matchInfo The aggregated match results. The keys should be
 * keys to the matching document.
 */
function createAnnotatedSearchResult(docIdx, matchInfo) {
  let doc = searchableContent[docIdx];

  function annotateEntireContent(originalMarkup, relevantMatchInfo) {
    if (!originalMarkup || !relevantMatchInfo) return "";

    let annotatedContent = "";
    let startIdx = 0;
    relevantMatchInfo.forEach((matchPos) => {
      annotatedContent += originalMarkup.substr(startIdx, matchPos[0] - startIdx);
      annotatedContent += `<span class="search-hit">`;
      annotatedContent += originalMarkup.substr(matchPos[0], matchPos[1]);
      annotatedContent += "</span>";
      startIdx = matchPos[0] + matchPos[1];
    });
    annotatedContent += originalMarkup.substr(startIdx);
    return annotatedContent;
  }

  // Annotate the matches in the title
  let annotatedTitle = annotateEntireContent(doc.title, matchInfo.title);

  // If the document does not exist in multiple places, hyperlink the title too
  if (!doc.equivalent_urls) {
    annotatedTitle = `<a href="${doc.url}">${annotatedTitle}</a>`;
  }
  
  // If the body matched, we want to show the results in context...
  let originalBody = doc.body;
  let annotatedBody = "";
  matchInfo.body.forEach((matchPos, k) => {
    let i = matchPos[0];
    while (i > 0 && originalBody[i] !== "\n") i -= 1;

    // Get the next two words after the match
    let j = matchPos[0] + matchPos[1];
    while (j < originalBody.length && originalBody[j] !== "\n") j += 1;

    if (k === 0) annotatedBody += "... ";
    annotatedBody += `${originalBody.slice(i, matchPos[0])}`;
    annotatedBody += `<span class="search-hit">`;
    annotatedBody += originalBody.substr(matchPos[0], matchPos[1]);
    annotatedBody += `</span>`;
    annotatedBody += `${originalBody.slice(matchPos[0] + matchPos[1], j + 1)}... `;

  });

  // Display all of the tags with the matches highlighted
  let annotatedTags = "";
  if (doc.tags) {
    let individualTags = doc.tags.split(/\s+/);
    let annotatedContent = annotateEntireContent(doc.tags, matchInfo.tags);
    individualTags.forEach((tag) => {
      annotatedContent = annotatedContent.replace(tag, `<a href="/tags/${tag}">${tag}</a>`)
    });
    annotatedTags = `Tags: ${annotatedContent}`;
  }

  // Display all URLs for the search result
  let urlMarkup = "";
  if (doc.equivalent_urls) {
    doc.equivalent_urls.forEach((url) => {
      // <p></p> to make the URLs appear on separate lines.
      urlMarkup += `<p class="meta"><a href="${url}">${url}</a></p>`; });
  } else {
    urlMarkup = `<a href="${doc.url}">${doc.url}</a>`;
  }
  
  return `<div>
    ${annotatedTitle}
    <p class="meta">
      ${DATE_FORMATTER.format((new Date(doc.date)))}; ${annotatedTags}
    </p>
    <p class="meta">${urlMarkup}</p>
    <p class="snippet">${annotatedBody}</p>
  </div>`;
}
  