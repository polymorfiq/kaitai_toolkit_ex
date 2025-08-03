defmodule KaitaiToolkit.KsyTest do
  use ExUnit.Case
  doctest KaitaiToolkit.Ksy

  import KaitaiToolkit.Ksy

  test "reads meta correctly" do
    [spec] = ~k"""
    meta:
      id: basic_save_file
      title: Example save file
      application: abc
      file-extension: .xyz
      xref:
        forensicswiki: https://forensics.wiki/something/
        iso: 15948:2004
        justsolve: http://fileformats.archiveteam.org/wiki/abc
        loc: fdd123456
        mime: image/png
        pronom: abcdef
        rfc: 1234
        wikidata: Q535473
      license: MIT
      ks-version: 1.1.1
      ks-debug: true
      ks-opaque-types: true
      imports:
        - abc.ksy
        - def.ksy
      encoding: UTF-8
      endian: le
    """

    assert spec.meta.id == "basic_save_file"
    assert spec.meta.title == "Example save file"
    assert spec.meta.application == ["abc"]
    assert spec.meta.file_extension == [".xyz"]
    assert spec.meta.xref.forensics_wiki == ["https://forensics.wiki/something/"]
    assert spec.meta.xref.iso == ["15948:2004"]
    assert spec.meta.xref.just_solve == ["http://fileformats.archiveteam.org/wiki/abc"]
    assert spec.meta.xref.loc == ["fdd123456"]
    assert spec.meta.xref.mime == ["image/png"]
    assert spec.meta.xref.pronom == ["abcdef"]
    assert spec.meta.xref.rfc == [1234]
    assert spec.meta.xref.wiki_data == ["Q535473"]
    assert spec.meta.license == "MIT"
    assert spec.meta.ks_version == "1.1.1"
    assert spec.meta.ks_debug == true
    assert spec.meta.ks_opaque_types == true
    assert spec.meta.imports == ["abc.ksy", "def.ksy"]
    assert spec.meta.encoding == "UTF-8"
    assert spec.meta.endian == :le
  end

  test "meta.id raises exception when incorrect format" do
    assert_raise RuntimeError, ~r/meta.id/, fn ->
      ~k"""
      meta:
        id: 1StartsWithNumber
      """
    end

    assert_raise RuntimeError, ~r/meta.id/, fn ->
      ~k"""
      meta:
        id: Capitalized
      """
    end

    assert_raise RuntimeError, ~r/meta.id/, fn ->
      ~k"""
      meta:
        id: hasCapitalization
      """
    end
  end

  test "doc is parsed correctly" do
    [spec] = ~k"""
    doc: This is a doc
    """

    assert spec.doc == "This is a doc"
  end

  test "doc_ref is parsed correctly" do
    [spec] = ~k"""
    doc-ref: https://www.myref.io
    """

    assert spec.doc_ref == ["https://www.myref.io"]
  end

  test "params is parsed correctly" do
    [spec] = ~k"""
    params:
      - id: a_param
        doc: My Doc
        doc-ref: My Doc Ref
        type: u8
      - id: b_param
        type: b123
      - id: c_param
        type: custom_type
    """

    assert [a, b, c] = spec.params
    assert a.id == "a_param"
    assert a.doc == "My Doc"
    assert a.doc_ref == ["My Doc Ref"]
    assert a.type == :u8

    assert b.id == "b_param"
    assert b.type == {:bits, 123}

    assert c.id == "c_param"
    assert c.type == {:user_defined, "custom_type"}
  end

  test "seq is parsed correctly" do
    [spec] = ~k"""
    seq:
      - id: a_attr
        doc: My Doc
        doc-ref: My Doc Ref
        type: u8le
      - id: b_attr
        type: b123
      - id: c_attr
        type: custom_type
    """

    assert [a, b, c] = spec.seq
    assert a.id == "a_attr"
    assert a.doc == "My Doc"
    assert a.doc_ref == ["My Doc Ref"]
    assert a.type == :u8le

    assert b.id == "b_attr"
    assert b.type == {:bits, 123}

    assert c.id == "c_attr"
    assert c.type == {:user_defined, "custom_type"}
  end
end
