cl-template: Simple general-purpose template engine for Common Lisp.
====================================================================

Getting cl-template
-------------------

cl-template will be available on quicklisp sooner or later. Until
then, just clone this repo and tell ASDF to find it. cl-template has
no dependencies.


Why cl-template?
----------------

cl-template is yet another template engine for Common Lisp, taking
inspiration from Ruby's [ERb module][1]. It differs from other
template engines in that:

- It can be used for any output. Most Common Lisp template engines
  devise some mapping from s-expressions to HTML. cl-template can
  generate HTML, JSON, CSV, Markdown, or any other text-based format.
- It's just Lisp. The only other output-agnostic template engines that
  I found, [CL-EMB][2] and the somewhat misnamed [HTML-TEMPLATE][3]
  have additional syntax. In the case of HTML-TEMPLATE it doesn't use
  Lisp at all, only special template directives. CL-EMB mostly uses
  Lisp, but has special directives like @if and @loop.


Syntax
------

The most distictive feature of cl-template is that it can infer the
outermost set of parenthesis, to make your templates look a little
cleaner. For example,

    <ul class="person-list">
      <% (loop for person in (@ people) do
        <li><%= (name person) %>
      <% ) %>
    </ul>
    
can be written as

    <ul class="person-list">
      <% loop for person in (@ people) do %>
        <li><%= name person %></li>
      <% end %>
    </ul>
    
The only deviations from pure Common Lisp are:
- Parenthesis inferrence
- `end` can function as a close parenthesis
- `progn` is automatically inserted into `if` expressions
- `else` separates clauses of an `if` expression

All of these are optional - you can choose not to use them at
all. However, they tend to make templates easier to read and are
recommended.

The rules for paren inference are:

- If it starts with an open paren, don't do anything.
- In a <%= %> block, if the content consists of sexps separated by
  spaces, assume it's a function call and add parenthesis at the
  beginning and end.
- In a <% %> block, always add parenthesis at the beginning and end if
  they aren't already there.
  
`@` is the macro to get a variable from the data provided at runtime.

Some more examples:

    {"articles": [
      <% loop for article in (@ articles) do %>
        {"id": <%= id article %>, "title": <%= title article %>, "body": <%= body article %>}
      <% end %>
    ]}
    
    <!-- Don't use any optional features, just plain Lisp. Be sure to set clt:*add-progn-to-if* to nil first. -->
    <% (if (comments (@ article)) %>
      <% (progn
      <ul id="article-<%= (id (@ article)) %>-comments" class="comments">
        <% (loop for comment in (comments (@ article)) do %>
        <li id="comment-<%= (id comment) %>" class="comment">
          <%= (author comment) %> said
          <p><%= (body comment) %></p>
        </li>
        <% ) %>
      </ul>
      <% ) %>
      <% (progn %>
      No comments! <a href="<%= link :new-comment %>">Add a comment.</a>
      <% ) %>
    <% ) %>


API
---

All of cl-template's functionality is exposed through one function,
`#'compile-template`.

    (compile-template string &key (start-delimiter "<%") (start-echo-delimiter "<%=") (end-delimiter "%>"))

This returns a lambda which must be called with a plist as the sole
argument, which provides the data for the template. This is available
within the template as `cl-template::__data` but it is not recommended
to use directly. (Although it is useful for things like iterating over
all data the template gets, but that sort of thing is rare.) A better
idea is to use the built-in `@` macro, which looks up a keyword
version of its argument in the data plist.

Usage example:

    (with-open-file (template-file "views/person.html.clt")
      (let ((template (make-string (file-length template-file)))
            (person (get-person-from-database-somehow)))
        (read-sequence template template-file)
        (funcall (cl-template:compile-template template) (list :person person))))

`#'compile-template` does not yet cache the resulting function.

Additionally, to disable automatically adding a `progn` to `if` expressions set `*add-progn-to-if*` to `nil`.

cl-template is licensed under the MIT license.    
Copyright (c) 2013 Peter Cannici


[1]: http://ruby-doc.org/stdlib/libdoc/erb/rdoc/ERB.html
[2]: http://common-lisp.net/project/cl-emb/
[3]: http://weitz.de/html-template/
