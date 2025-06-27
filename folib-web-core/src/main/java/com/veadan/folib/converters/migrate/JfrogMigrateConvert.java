package com.veadan.folib.converters.migrate;

import com.veadan.folib.entity.UserGroup;
import com.veadan.folib.users.dto.UserDto;
import org.jfrog.artifactory.client.model.Group;
import org.jfrog.artifactory.client.model.User;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

/**
 * @author veadan
 * @since 2024-10-23 10:43
 */

@Mapper
public interface JfrogMigrateConvert {

    JfrogMigrateConvert INSTANCE = Mappers.getMapper(JfrogMigrateConvert.class);

    @Mapping(source = "name", target = "groupName")
    @Mapping(target = "joinGroup" ,expression="java(group.isAutoJoin()?\"1\":\"0\")")
    @Mapping(target = "isDefault", constant = "0")
    @Mapping(target = "deleted", constant = "0")
    UserGroup jfrogGroupToFolib(Group group);


}
