mechanizerl
===========

A clone of WWW::Mechanize for Erlang, with perhaps a bit of
Test::WWW::Mechanize and Webtest for good measure.  Named according to
the seeming defacto convention of inserting "erl" in silly places in
the name.  This is primarily a convenience wrapper around httpc.

Examples:
---------

.. code-block:: erlang

   erl -pa ebin -boot start_sasl -s inets -s xmerl -s mechanizerl

   1> Mech = mechanizerl:new().
   {mechanizerl,<0.64.0>}
   2> Mech:get("http://google.com/").
   ok
   3> Mech:submit_form("search", [{q, "mechanizerl"}]).
   ok
   4> Mech:body().
   <<"<html>...">>

Caveats:
--------

* The interface isn't as Erlangy as it could be (it uses the lowly
  tuple calls), as I tried to follow the WWW::Mechanize API closely to
  facilitate translating old tests I had written in perl into Erlang.
  Part of the efficacy of Mechanize's interface is its statefulness.

* This uses xmerl to parse the HTML it receives.  This works for me,
  since I tend to write HTML in an XHTML-ish fashion, but be warned that
  it will choke on lots of other HTML.  I considered using one of the
  HTML parsers from YAWS or Mochiweb, but it seemed unnecessary for my
  purposes.

* Strings are used throughout rather than binaries, for convenience
  rather than efficiency.

* I've only implemented so far the subset of WWW::Mechanize's
  functionality that I actually use.  For example, cookies are always
  enabled.  Patches Thoughtfully Considered.

* Lists are used in numerous places where more sophisticated data
  structures would be more efficient.  Making the simplest thing that
  would work trumped efficiency in this first version.

To do:
------

* Enforce constraints on fields (select options, maxlength, et
  cetera);

* More extensive testing: replay-based testing of redirects, et
  cetera;

* File upload;

* Examples and documentation.

Contact
-------

Julian Squires <julian@cipht.net>
