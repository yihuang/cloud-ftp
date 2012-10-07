This is a FTP server which use cloud storage service as backend, currently only support aliyun OSS service.

How to install
==============

::

    $ cd path/to/project
    $ cabal install

How to run
==========

::

    $ cat path/to/config
    [username]
    password = password
    service  = aliyun
    host     = storage.aliyun.com
    identity = ALIYUN_IDENTITY
    key      = ALIYUN_KEY
    $ cloud-ftp-server port-number path/to/config

Then use the username specified in config file to login this ftp server.

In the top directory, you can see all the buckets you own, enter one bucket, you can see the file and directory in it.
