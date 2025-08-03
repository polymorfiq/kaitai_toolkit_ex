defmodule KaitaiToolkit.Ksy.Meta do
  alias KaitaiToolkit.Ksy.Xref

  defstruct [
    :id,
    :title,
    :application,
    :file_extension,
    :xref,
    :license,
    :ks_version,
    :ks_debug,
    :ks_opaque_types,
    :imports,
    :encoding,
    :endian
  ]

  @type t :: %__MODULE__{
          id: String.t(),
          title: String.t() | nil,
          application: String.t() | [String.t()],
          file_extension: String.t() | [String.t()],
          xref: Xref.t() | nil,
          license: String.t() | nil,
          ks_version: String.t() | nil,
          ks_debug: boolean(),
          ks_opaque_types: boolean(),
          imports: [String.t()],
          encoding: String.t() | nil,
          endian: :le | :be | %{switch_on: term(), cases: tuple()} | nil
        }

  @spec from_map!(map()) :: t()
  def from_map!(data) when is_map(data) do
    %__MODULE__{
      id: id(data["id"]),
      title: title(data["title"]),
      application: application(data["application"]),
      file_extension: file_extension(data["file-extension"]),
      xref: xref(data["xref"]),
      license: license(data["license"]),
      ks_version: ks_version(data["ks-version"]),
      ks_debug: ks_debug(data["ks-debug"]),
      ks_opaque_types: ks_opaque_types(data["ks-opaque-types"]),
      imports: imports(data["imports"]),
      encoding: encoding(data["encoding"]),
      endian: endian(data["endian"])
    }
  end

  defp id(id) when is_binary(id) do
    if Regex.match?(~r|^[a-z][a-z0-9_]*$|, id) do
      id
    else
      raise "KSY Parsing - meta.id (#{id}) does not match expected regex (^[a-z][a-z0-9_]*$)"
    end
  end

  defp title(nil), do: nil
  defp title(title) when is_binary(title), do: title

  defp application(nil), do: []
  defp application(app) when is_binary(app), do: [app]
  defp application(apps) when is_list(apps), do: apps

  defp file_extension(nil), do: []
  defp file_extension(ext) when is_binary(ext), do: [ext]
  defp file_extension(exts) when is_list(exts), do: exts

  defp xref(nil), do: nil
  defp xref(xref) when is_map(xref), do: Xref.from_map!(xref)

  defp license(nil), do: nil
  defp license(license) when is_binary(license), do: license

  defp ks_version(nil), do: nil
  defp ks_version(version) when is_binary(version), do: version

  defp ks_debug(nil), do: false
  defp ks_debug(debug) when is_boolean(debug), do: debug

  defp ks_opaque_types(nil), do: false
  defp ks_opaque_types(opaque_types) when is_boolean(opaque_types), do: opaque_types

  defp imports(nil), do: []
  defp imports(imports) when is_list(imports), do: imports

  defp encoding(nil), do: nil
  defp encoding(encoding) when is_binary(encoding), do: encoding

  defp endian(nil), do: nil
  defp endian("le"), do: :le
  defp endian("be"), do: :be

  defp endian(%{"switch-on" => switch_on, "cases" => cases}) do
    cases =
      cases
      |> Enum.map(fn
        {key, "le"} -> {key, :le}
        {key, "be"} -> {key, :be}
      end)

    %{switch_on: switch_on, cases: cases}
  end
end
