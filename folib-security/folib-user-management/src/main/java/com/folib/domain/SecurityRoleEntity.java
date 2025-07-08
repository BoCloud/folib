package com.folib.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.folib.data.domain.DomainEntity;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.neo4j.ogm.annotation.NodeEntity;

import static com.veadan.folib.db.schema.Vertices.SECURITY_ROLE;

/**
 * @author veadan
 */
@NodeEntity(SECURITY_ROLE)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class SecurityRoleEntity extends DomainEntity implements SecurityRole
{
    private String roleName;
    @JsonIgnore
    private Long nativeId;
    private String uuid;

    public SecurityRoleEntity(String role)
    {
        setUuid(role);
        setRoleName(role);
    }

    @Override
    public String getUuid()
    {
        return uuid;
    }

    public void setUuid(String uuid)
    {
        if (this.uuid != null && !this.uuid.equals(uuid))
        {
            throw new IllegalStateException(String.format("Can't change the uuid, [%s]->[%s].", this.uuid, uuid));
        }

        this.uuid = uuid;
    }
}
