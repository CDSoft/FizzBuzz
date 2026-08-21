--[[
one_h1.lua

Pandoc Lua filter that, but ONLY when the document contains more than
one level-1 header:
  1. Increments the level of every existing header in the document
     (a level-1 header becomes level 2, a level-2 header becomes level 3, etc.)
  2. Inserts a new level-1 header at the top of the document, whose text
     comes from the "title" field in the YAML metadata.

If the document has zero or exactly one level-1 header, it is left
untouched (this covers the common case of a document that already has
a single top-level title and does not need restructuring).

Usage:
  pandoc --lua-filter=one_h1.lua input.md -o output.pdf
--]]

local MAX_LEVEL = 6

-- Increment the level of each header encountered (capped at 6, Pandoc's limit)
local function increment_header(el)
    el.level = math.min(el.level + 1, MAX_LEVEL)
    return el
end

-- Count how many level-1 headers are present anywhere in the document
local function count_level1_headers(doc)
    local count = 0
    doc:walk {
        Header = function(el)
            if el.level == 1 then
                count = count + 1
            end
        end
    }
    return count
end

-- Convert the "title" metadata value into a list of Inlines,
-- preserving formatting if present (bold, italics, etc.)
local function title_to_inlines(meta_title)
    if meta_title == nil then
        return nil
    elseif meta_title.t == "MetaInlines" then
        -- Standard case: title: My Title (possibly with inline formatting)
        return pandoc.Inlines(meta_title)
    elseif meta_title.t == "MetaBlocks" then
        -- Rare case: title contains blocks (e.g. multiple lines) -> flatten to text
        local text = pandoc.utils.stringify(meta_title)
        return pandoc.Inlines { pandoc.Str(text) }
    else
        -- MetaString or other type: convert to plain text
        local text = pandoc.utils.stringify(meta_title)
        return pandoc.Inlines { pandoc.Str(text) }
    end
end

function Pandoc(doc)
    -- Only restructure the document if it has more than one level-1 header
    if count_level1_headers(doc) <= 1 then
        return doc
    end

    -- 1) Increment all existing headers in the document body
    local incremented_doc = doc:walk { Header = increment_header }

    -- 2) Build the new H1 header from the metadata
    local title_inlines = title_to_inlines(doc.meta.title)

    if title_inlines then
        local new_title = pandoc.Header(1, title_inlines)
        table.insert(incremented_doc.blocks, 1, new_title)
    else
        io.stderr:write "one_h1.lua: no 'title' field found in YAML metadata, no H1 header was added.\n"
    end

    return incremented_doc
end
