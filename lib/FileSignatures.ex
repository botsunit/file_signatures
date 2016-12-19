# File: FileSignatures.ex
# This file was generated from file_signatures.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule FileSignatures do
  def unquote(:"is_type")(arg1, arg2) do
    :erlang.apply(:"file_signatures", :"is_type", [arg1, arg2])
  end
  def unquote(:"is_valid")(arg1) do
    :erlang.apply(:"file_signatures", :"is_valid", [arg1])
  end
  def unquote(:"signature")(arg1) do
    :erlang.apply(:"file_signatures", :"signature", [arg1])
  end
end
