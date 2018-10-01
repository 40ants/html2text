=================
 html2text
=================

.. insert-your badges like that:

.. image:: https://travis-ci.org/40ants/html2text.svg?branch=master
    :target: https://travis-ci.org/40ants/html2text

.. Everything starting from this commit will be inserted into the
   index page of the HTML documentation.
.. include-from

WORK IN PROGRESS.

Reasoning
=========

There is great html2text python library, but I didn't found a way how to
convert HTML to Markdown from Common Lisp. And made this library.

Example:

.. code-block:: common-lisp-repl

   CL-USER> (html2text:html2text "
   <p>A paragraph
   which can be <b>multiline</b> and <em>can contain other tags</em>.
   </p>
   
   <ul>
     <li>Lisp is a great language</li>
     <li>For the reason</li>
   </ul>
   ")
   "A paragraph which can be **multiline** and _can contain other tags_ .
   
   * Lisp is a great language
   * For the reason
   "
   CL-USER> 


.. Everything after this comment will be omitted from HTML docs.
.. include-to

Building Documentation
======================

Provide instruction how to build or use your library.

How to build documentation
--------------------------

To build documentation, you need a Sphinx. It is
documentaion building tool written in Python.

To install it, you need a virtualenv. Read
this instructions
`how to install it
<https://virtualenv.pypa.io/en/stable/installation/#installation>`_.

Also, you'll need a `cl-launch <http://www.cliki.net/CL-Launch>`_.
It is used by documentation tool to run a script which extracts
documentation strings from lisp systems.

Run these commands to build documentation::

  virtualenv --python python2.7 env
  source env/bin/activate
  pip install -r docs/requirements.txt
  invoke build_docs

These commands will create a virtual environment and
install some python libraries there. Command ``invoke build_docs``
will build documentation and upload it to the GitHub, by replacing
the content of the ``gh-pages`` branch.


Authors
=======

* Alexander Artemenko (svetlyak.40wt@gmail.com)

Copyright
=========

Copyright (c) 2018 Alexander Artemenko (svetlyak.40wt@gmail.com)

License
=======

Licensed under the BSD License.
