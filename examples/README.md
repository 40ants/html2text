This directory contains a `test.html` file and results of it's processing by different `html2text` processors.
Here is how these results were produced:

### cl-test.md

    roswell/html2text.ros examples/test.html > examples/cl-test.md
    
    
### python-test.md

    virtualenv env
    env/bin/pip install html2text
    env/bin/html2text examples/test.html > examples/python-test.md
    
### Add more implementations

If you know other processors, please, add them here and send a pull-request.
