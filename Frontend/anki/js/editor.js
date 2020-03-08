// anki editor
//
// suppose only one editor-** exist.

var editorPanel = document.getElementsByClassName("editor-panel")[0]
var textarea = document.getElementsByClassName("editor-input")[0]
var textLayer = document.getElementsByClassName("editor-layer-text")[0]
var cursorLayer = document.getElementsByClassName("editor-layer-cursor")[0]
function currentLine() { return document.getElementsByClassName("editor-current-line")[0] }

//┌─────────────────────┐
//│ constant for config │
//└─────────────────────┘
var fontFamily = "Consolas,Monaco,monospace"
// fontsize is linked to height of .editor-text-line
// to set fontsize of textLayer, use this function please
function fontSize() {
    let c = currentLine();
    return (parseInt(c.style.height) - parseInt(c.style.paddingTop) * 2) + "px"
}

//┌───────────────────────────┐
//│ set the config for styles │
//└───────────────────────────┘
function setFont() {
    editorPanel.style.fontFamily = fontFamily
    textLayer.style.fontSize = fontSize()
}

textarea.oninput = (e) => {
    let c = currentLine()
    if (c.children.length === 0) {
        c.innerHTML='<div></div>'
    }
    c.children.item(0).innerText += e.data
    let lineWidth = c.children.item(0).clientWidth
    
    cursorLayer.style.left = lineWidth + "px"
    textarea.style.left = lineWidth + "px"
}

//┌───────────────────┐
//│ run commands here │
//└───────────────────┘
setFont()
