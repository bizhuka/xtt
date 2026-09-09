CLASS lcl_helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS pretty_print
      IMPORTING
        iv_xml         TYPE string
        iv_indent_size TYPE i DEFAULT 2
      RETURNING
        VALUE(rv_xml)  TYPE string.
ENDCLASS.

CLASS lcl_helper IMPLEMENTATION.

  METHOD pretty_print.
    WRITE '@KERNEL let xml = iv_xml.get();'.
    WRITE '@KERNEL let indentSize = iv_indent_size.get();'.
    WRITE '@KERNEL let indent = " ".repeat(indentSize);'.
    WRITE '@KERNEL let depth = 0;'.

    WRITE '@KERNEL let tokens = xml'.
    WRITE '@KERNEL   .replace(/>\s+</g, "><")'.
    WRITE '@KERNEL   .trim()'.
    WRITE '@KERNEL   .match(/(<\[CDATA\[.*?\]\]>|<!--.*?-->|<[^>]+>|[^<]+)/gs) || [];'.

    WRITE '@KERNEL let formatted = tokens.map(token => {'.
    WRITE '@KERNEL   if (!token.trim()) return "";'.
    WRITE '@KERNEL   if (token.startsWith("<!--") || token.startsWith("<![CDATA[")) {'.
    WRITE '@KERNEL     return indent.repeat(depth) + token.trim();'.
    WRITE '@KERNEL   }'.
    WRITE '@KERNEL   if (token.match(/^<[^>]+?\/>$/) || token.startsWith("<?")) {'.
    WRITE '@KERNEL     return indent.repeat(depth) + token;'.
    WRITE '@KERNEL   }'.
    WRITE '@KERNEL   if (token.startsWith("</")) {'.
    WRITE '@KERNEL     depth = Math.max(0, depth - 1);'.
    WRITE '@KERNEL     return indent.repeat(depth) + token;'.
    WRITE '@KERNEL   }'.
    WRITE '@KERNEL   if (token.startsWith("<")) {'.
    WRITE '@KERNEL     let line = indent.repeat(depth) + token;'.
    WRITE '@KERNEL     depth++;'.
    WRITE '@KERNEL     return line;'.
    WRITE '@KERNEL   }'.
    WRITE '@KERNEL   return indent.repeat(depth) + token.trim();'.
    WRITE '@KERNEL }).filter(line => line.length > 0).join("\n");'.

    WRITE '@KERNEL rv_xml.set(formatted);'.
  ENDMETHOD.
ENDCLASS.
