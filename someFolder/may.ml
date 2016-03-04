let getSubstringMaybe s idx =
  try Some (Pcre.get_substring s idx)
  with Not_found | Invalid_argument _ -> None
