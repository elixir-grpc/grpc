defmodule ExceptionServer do
  use GenServer

  # Callbacks

  @impl true
  def init(pid) do
    {:ok, pid}
  end

  @impl true
  def handle_cast(:case_boom, state) do
    a = fn -> :ok end

    case a.() do
      :error -> :boom
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast(:bad_arg_boom, state) do
    ets = :ets.new(:foo, [])
    :ets.delete(ets)
    :ets.insert(ets, 1)

    {:noreply, state}
  end

  @impl true
  def terminate(reason, state) do
    send(state, {:boom, reason})
    :ok
  end
end

defmodule GRPC.Server.Adapters.ReportExceptionTest do
  use ExUnit.Case, async: true
  alias GRPC.Server.Adapters.ReportException

  describe "new/3" do
    test "with runtime error" do
      assert %GRPC.Server.Adapters.ReportException{
               __exception__: true,
               adapter_extra: [req: :ok],
               kind: :error,
               reason: %RuntimeError{message: "hi", __exception__: true},
               stack: []
             } == ReportException.new([req: :ok], RuntimeError.exception("hi"))
    end

    test "with case clause error" do
      {:ok, pid} = GenServer.start_link(ExceptionServer, self())

      GenServer.cast(pid, :case_boom)

      receive do
        {:boom, {_reason, stack} = err} ->
          assert %GRPC.Server.Adapters.ReportException{
                   __exception__: true,
                   adapter_extra: [req: :ok],
                   kind: :error,
                   reason: %CaseClauseError{term: :ok},
                   stack: stack
                 } == ReportException.new([{:req, :ok}], err)
      end
    end

    test "with badarg error" do
      {:ok, pid} = GenServer.start_link(ExceptionServer, self())

      GenServer.cast(pid, :bad_arg_boom)

      receive do
        {:boom, {_reason, stack} = err} ->
          assert %GRPC.Server.Adapters.ReportException{
                   __exception__: true,
                   adapter_extra: [req: :ok],
                   kind: :error,
                   reason: %ArgumentError{
                     __exception__: true,
                     message:
                       "errors were found at the given arguments:\n\n  * 1st argument: the table identifier does not refer to an existing ETS table\n  * 2nd argument: not a tuple\n"
                   },
                   stack: stack
                 } == ReportException.new([{:req, :ok}], err)
      end
    end
  end
end
