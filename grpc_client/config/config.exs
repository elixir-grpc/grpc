import Config

config_file = Path.expand("#{config_env()}.exs", __DIR__)

if File.exists?(config_file) do
  import_config config_file
end
