package com.veadan.folib.converters.users;

import com.veadan.folib.dto.FolibRoleDTO;
import com.veadan.folib.dto.RoleDTO;
import com.veadan.folib.entity.FolibRole;
import com.veadan.folib.dto.users.auth.RoleDto;
import org.mapstruct.Mapper;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

import java.util.List;

/**
 * @Author: fengmaogen
 * @Date: 2024/7/16 19:23
 * @Description: UserGroupConvert
 */
@Mapper
public interface RoleConvert {

    RoleConvert INSTANCE = Mappers.getMapper(RoleConvert.class);

    @Mappings({})
    RoleDTO formToDto(RoleDto roleForm);

    @Mappings({})
    List<FolibRole> roleDTOSToEntities(List<FolibRoleDTO> roleDTOS);
}
