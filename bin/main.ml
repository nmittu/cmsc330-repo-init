open! Core

let () =
  let gradescope_token = Gradescope.get_signed_token () in
  let github_auth = Github.get_auth () in
  let github_repo = Github.get_current_repo github_auth in
  if Github.set_secret github_repo ~key:"GRADESCOPE_TOKEN" ~value:gradescope_token then
    printf "Successfully set up github repo\n"
  else
    printf "There was an issue setting up your github repo. Please see a TA\n"
