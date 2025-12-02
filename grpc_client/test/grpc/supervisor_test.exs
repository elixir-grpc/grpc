defmodule GRPC.Client.SupervisorTest do
  use ExUnit.Case, async: false

  alias GRPC.Client

  describe "start_link/1" do
    test "allows multiple start_links" do
      {:ok, second_pid} = Client.Supervisor.start_link([])
      {:ok, third_pid} = Client.Supervisor.start_link([])

      assert second_pid == third_pid
    end
  end
end
