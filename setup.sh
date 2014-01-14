# This does any extra work to set up the emacs configuration environment. In general, you should run this after fetching changes to the config.
bash -c "cd modules/traad/traad; source venv2/bin/activate; python setup.py install"
bash -c "cd modules/traad/traad; source venv3/bin/activate; python setup.py install"

