-module(test).
-compile(export_all).

test1() ->
  js_links_machine:run_statem(void,["question.jsch","answer.jsch","statement.jsch","reset.jsch","answer_reply.jsch"]).

test2() ->
  js_links_machine:run_statem(qa,["question.jsch","answer.jsch","statement.jsch","reset.jsch","answer_reply.jsch"]).

test3() ->
  js_links_machine:run_statem(qa,["question.jsch","answer.jsch","statement.jsch","reset.jsch","answer_reply.jsch"],[cookies]).

  
