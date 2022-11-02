'use strict';

// Original: https://github.com/dracula/visual-studio-code
// Converted automatically using ./tools/themeFromVsCode
// modified by me later
var theme = {
    plain: {
        color: "#F8F8F2",
        backgroundColor: "#282A36"
    },
    styles: [{
        types: ["prolog", "constant", "builtin"],
        style: {
            color: "#9393f9"
        }
    }, {
        types: ["number", "inserted", "function"],
        style: {
            color: "#93B8F5"
        }
    }, {
        types: ["deleted"],
        style: {
            color: "rgb(255, 85, 85)"
        }
    }, {
        types: ["changed"],
        style: {
            color: "rgb(255, 184, 108)"
        }
    }, {
        types: ["punctuation", "symbol"],
        style: {
            color: "rgb(248, 248, 242)"
        }
    }, {
        types: ["string", "char", "tag", "selector"],
        style: {
            color: "rgb(255, 121, 198)"
        }
    }, {
        types: ["keyword", "variable", "boolean"],
        style: {
            color: "rgb(189, 147, 249)",
        }
    }, {
        types: ["comment"],
        style: {
            color: "rgb(98, 114, 164)"
        }
    }, {
        types: ["attr-name"],
        style: {
            color: "rgb(241, 250, 140)"
        }
    }]
};

module.exports = theme;