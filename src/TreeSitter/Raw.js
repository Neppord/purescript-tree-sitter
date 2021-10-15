const Parser = require("tree-sitter")

exports.mkParser = Parser
exports.mkLanguage = languageString => require("tree-sitter-" + languageString)
exports.noTree = undefined
exports.noOptions = undefined
exports.toMaybeTree = t => t
exports.toMaybeOptions = o => o