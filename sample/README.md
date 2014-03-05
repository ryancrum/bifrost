# Bifrost Sample App

This is a simple sample application using the Bifrost FTP framework. It uses a very stupid backend that simply stores files in an in-memory database that only lasts as long as the current FTP client session. It's only intended to demonstrate how to implement a Bifrost server.

Run it by starting the bifrost_sample application (i.e. `application:start(bifrost_sample).`). It will launch an FTP server on port 2121. Any username/password combination is accepted.
