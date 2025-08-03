defmodule KaitaiToolkit.Ksy.Xref do
  defstruct [
    :forensics_wiki,
    :iso,
    :just_solve,
    :loc,
    :mime,
    :pronom,
    :rfc,
    :wiki_data
  ]

  @type t :: %__MODULE__{
          forensics_wiki: [String.t()],
          iso: [String.t()],
          just_solve: [String.t()],
          loc: [String.t()],
          mime: [String.t()],
          pronom: [String.t()],
          rfc: [String.t()],
          wiki_data: [String.t()]
        }

  @spec from_map!(map()) :: t()
  def from_map!(data) when is_map(data) do
    %{
      forensics_wiki: forensics_wiki(data["forensicswiki"]),
      iso: iso(data["iso"]),
      just_solve: just_solve(data["justsolve"]),
      loc: loc(data["loc"]),
      mime: mime(data["mime"]),
      pronom: pronom(data["pronom"]),
      rfc: rfc(data["rfc"]),
      wiki_data: wiki_data(data["wikidata"])
    }
  end

  defp forensics_wiki(nil), do: []
  defp forensics_wiki(wiki) when is_binary(wiki), do: [wiki]
  defp forensics_wiki(wikis) when is_list(wikis), do: wikis

  defp iso(nil), do: []
  defp iso(iso) when is_binary(iso), do: [iso]
  defp iso(isos) when is_list(isos), do: isos

  defp just_solve(nil), do: []
  defp just_solve(just_solve) when is_binary(just_solve), do: [just_solve]
  defp just_solve(just_solves) when is_list(just_solves), do: just_solves

  defp loc(nil), do: []
  defp loc(loc) when is_binary(loc), do: [loc]
  defp loc(locs) when is_list(locs), do: locs

  defp mime(nil), do: []
  defp mime(mime) when is_binary(mime), do: [mime]
  defp mime(mimes) when is_list(mimes), do: mimes

  defp pronom(nil), do: []
  defp pronom(pronom) when is_binary(pronom), do: [pronom]
  defp pronom(pronoms) when is_list(pronoms), do: pronoms

  defp rfc(nil), do: []
  defp rfc(rfc) when is_integer(rfc), do: [rfc]
  defp rfc(rfc) when is_binary(rfc), do: [String.to_integer(rfc)]
  defp rfc(rfcs) when is_list(rfcs), do: Enum.map(rfcs, &rfc/1)

  defp wiki_data(nil), do: []
  defp wiki_data(wiki) when is_binary(wiki), do: [wiki]
  defp wiki_data(wikis) when is_list(wikis), do: wikis
end
