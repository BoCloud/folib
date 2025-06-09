sudo su -
groupadd folib
useradd -d /usr/local/folib -g folib folib
chown -R folib:folib /usr/local/folib/

tar -zxf folib-distribution-2.0-SNAPSHOT.tar.gz -C /usr/local/folib --strip-components=1
ln -s /usr/local/folib/bin/wrapper-linux-x86-64 /usr/local/folib/bin/wrapper
