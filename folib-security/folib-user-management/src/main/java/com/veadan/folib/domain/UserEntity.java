package com.veadan.folib.domain;

import static com.veadan.folib.db.schema.Vertices.USER;
import static org.neo4j.ogm.annotation.Relationship.OUTGOING;
import static com.veadan.folib.db.schema.Edges.USER_HAS_SECURITY_ROLES;

import com.veadan.folib.data.domain.DomainEntity;
import com.veadan.folib.gremlin.adapters.DateConverter;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;

import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.Relationship;
import org.neo4j.ogm.annotation.typeconversion.Convert;

/**
 * @author sbespalov
 *
 */
@NodeEntity(USER)
public class UserEntity extends DomainEntity implements User
{

    private String password;

    private Boolean enabled = true;

    @Relationship(type = USER_HAS_SECURITY_ROLES, direction = OUTGOING)
    private Set<SecurityRole> roles = new HashSet<>();

    private String securityTokenKey;

    @Convert(DateConverter.class)
    private LocalDateTime lastUpdated;

    private String sourceId;

    UserEntity()
    {
    }

    public UserEntity(String username)
    {
        setUuid(username);
    }

    @Override
    public String getPassword()
    {
        return password;
    }

    public void setPassword(String password)
    {
        this.password = password;
    }

    @Override
    public Set<SecurityRole> getRoles()
    {
        return roles;
    }

    public void setRoles(Set<SecurityRole> roles)
    {
        this.roles = roles != null ? new HashSet<>(roles) : new HashSet<>();
    }

    public void addRole(String role)
    {
        addRole(new SecurityRoleEntity(role));
    }

    public void addRole(SecurityRole role)
    {
        roles.add(role);
    }

    public void removeRole(SecurityRole role)
    {
        roles.remove(role);
    }

    public boolean hasRole(SecurityRole role)
    {
        return roles.contains(role);
    }

    @Override
    public String getSecurityTokenKey()
    {
        return securityTokenKey;
    }

    public void setSecurityTokenKey(String securityTokenKey)
    {
        this.securityTokenKey = securityTokenKey;
    }

    @Override
    public Boolean isEnabled()
    {
        return enabled;
    }

    public void setEnabled(final Boolean enabled)
    {
        this.enabled = enabled;
    }

    @Override
    public LocalDateTime getLastUpdated()
    {
        return lastUpdated;
    }

    public void setLastUpdated(LocalDateTime lastUpdated)
    {
        this.lastUpdated = lastUpdated;
    }

    public String getSourceId()
    {
        return sourceId;
    }

    public void setSourceId(String source)
    {
        this.sourceId = source;
    }

}
