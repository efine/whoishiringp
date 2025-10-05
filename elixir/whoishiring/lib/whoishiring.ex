defmodule WhoIsHiring do
  @url_base "https://hacker-news.firebaseio.com/v0"
  @user_id "whoishiring"

  def main(max_concurrency) do
    Application.ensure_all_started(:inets)
    Application.ensure_all_started(:ssl)
    user = get_user(@url_base, @user_id)

    case lookup(user, "submitted") do
      nil ->
        IO.puts("No submissions for user ~p\n", [User])

      submissions ->
        print_jobs(@url_base, submissions, max_concurrency)
    end
  end

  def print_jobs(jobs) do
    numbered_jobs = [Enum.to_list(1..length(jobs)), jobs] |> List.zip()
    for {n, job} <- numbered_jobs, do: IO.puts("##{n}: #{job}\n")
    :ok
  end

  def print_jobs(url_base, submissions, max_concurrency) do
    case get_first_matching_submission(url_base, submissions) do
      {:ok, kw} ->
        title = lookup(kw, "title", "")
        kids = lookup(kw, "kids", [])
        IO.puts(title)

        pget_jobs(url_base, kids, max_concurrency)
        |> Enum.reject(&(!&1))
        |> print_jobs()

      _Else ->
        :ok
    end
  end

  def get_first_matching_submission(url_base, [id | submitted]) do
    submission = get_url_json(url_base, "item", id)

    case is_matching_submission(submission) do
      true ->
        {:ok, submission}

      false ->
        get_first_matching_submission(url_base, submitted)
    end
  end

  def get_first_matching_submission(_url_base, []) do
    nil
  end

  def is_matching_submission(nil), do: false

  def is_matching_submission(submission) do
    title = lookup(submission, "title", "")
    type = lookup(submission, "type", "")
    title =~ ~r/Ask\s+HN:\s+Who\s+is\s+hiring/i and type == "story"
  end

  def pget_jobs(url_base, kids, chunk_size) do
    IO.puts("Found #{length(kids)} jobs to search\n")
    pmap_n(WhoIsHiring, :get_job, [url_base], kids, chunk_size)
  end

  def pmap_n(mod, func_name, extra_args, list, max_chunk, results \\ [], n \\ 0)

  def pmap_n(_, _, _, [], _, results, n) do
    :io.format(:standard_error, '\r~8..0B\n', [n])

    results
    |> Enum.reject(&(&1 == nil))
    |> Enum.reverse()
  end

  def pmap_n(mod, func_name, extra_args, list, max_chunk, results, n) do
    case Enum.split(list, max_chunk) do
      {[_ | _] = todo, rest} ->
        rev_results =
          pmap(mod, func_name, extra_args, todo)
          |> Enum.reverse()

        n1 = n + length(todo)
        :io.format(:standard_error, '~8..0B\r', [n1])
        pmap_n(mod, func_name, extra_args, rest, max_chunk, rev_results ++ results, n1)

      {[], rest} ->
        pmap_n(mod, func_name, extra_args, rest, max_chunk, results, n)
    end
  end

  def pmap(mod, func_name, extra_args, args) do
    results =
      for arg <- args do
        Task.async(mod, func_name, [arg | extra_args])
      end
      |> Task.yield_many(30000)
      |> Enum.map(fn {task, result} ->
        result || Task.shutdown(task, :brutal_kill)
      end)

    for {:ok, result} <- results, do: result
  end

  # #Synchronous http call
  def get_url_json(base, resource, id, timeout \\ 30000) do
    {:ok, request_id} = get_url_json_async(base, resource, id)

    case receive_response(request_id, timeout) do
      {:ok, response} -> response
      {:error, _} -> nil
    end
  end

  # Asynchronous http call
  def get_url_json_async(base, resource, id) do
    url = make_url(base, resource, id)
    opts = [{:body_format, :binary}, {:sync, false}]
    :httpc.request(:get, {url, []}, [], opts)
  end

  def receive_response(request_id, timeout) do
    receive do
      {:http, {^request_id, result}} ->
        {{_version, 200, _reason_phrase}, _headers, body} = result
        {:ok, :jsx.decode(body)}
    after
      timeout ->
        {:error, :timeout}
    end
  end

  def make_url(base, resource, id) do
    "#{base}/#{resource}/#{id}.json" |> to_charlist
  end

  def check_remote_job(nil), do: false
  def check_remote_job(text), do: text =~ ~r/remote/i && text

  def get_job(id, url_base) when is_integer(id) do
    get_url_json(url_base, "item", id)
    |> get_text()
    |> check_remote_job()
  end

  def get_text(kid) when is_list(kid) do
    case {lookup(kid, "text"), lookup(kid, "deleted")} do
      {_, true} ->
        nil

      {nil, _} ->
        IO.puts("No text in #{inspect(kid)}\n")
        nil

      {text, _} ->
        text
    end
  end

  def get_text(_) do
    nil
  end

  def get_user(url_base, user_id) do
    user = get_url_json(url_base, "user", user_id)
    id = lookup(user, "id")

    case id == user_id do
      false ->
        msg = "Expected user id [#{user_id}] in #{inspect(user)}, got [#{id}], aborting"
        IO.puts(msg)
        throw(msg)

      true ->
        user
    end
  end

  def lookup(kvs, k, default \\ nil) when is_list(kvs) do
    case List.keyfind(kvs, k, 0) do
      nil -> default
      tval -> elem(tval, 1)
    end
  end
end
