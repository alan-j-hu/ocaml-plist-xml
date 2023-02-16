let source ?(buf_size = 256) source =
  let cstruct = Cstruct.create buf_size in
  let cursor = ref 0 in
  let limit = ref 0 in
  fun () ->
    let n = !cursor in
    if n < !limit then (
      cursor := n + 1;
      Cstruct.get_uint8 cstruct n)
    else
      let bytes_read = Eio.Flow.single_read source cstruct in
      limit := bytes_read;
      cursor := 1;
      Cstruct.get_uint8 cstruct 0
