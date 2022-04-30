# What's this all about?

The path `./resources/ldap` contains the `folib-generic.ldif` which is used both in UnboundID and OpenLDAP via 
Docker Compose.

The file `folib-base.ldif` is mounted "as-is" into the container and is imported into OpenLDAP upon startup. 
Might be worth visiting [osxia/openldap](https://github.com/osixia/docker-openldap#osixiaopenldap).

More about how to test using OpenLDAP can be found in [folib-ldap-authentication-provider's readme](../../../../../../folib-security/folib-authentication-providers/folib-ldap-authentication-provider/README.md).
