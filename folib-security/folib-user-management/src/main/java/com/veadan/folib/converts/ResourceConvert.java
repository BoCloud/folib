package com.veadan.folib.converts;

import com.veadan.folib.dto.AccessResourcesDTO;
import com.veadan.folib.entity.Resource;
import org.mapstruct.Mapper;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

import java.util.List;

/**
 * @Author: fengmaogen
 * @Date: 2024/7/16 19:23
 * @Description: ResourceConvert
 */
@Mapper
public interface ResourceConvert {

    ResourceConvert INSTANCE = Mappers.getMapper(ResourceConvert.class);

    @Mappings({})
    Resource formToDto(AccessResourcesDTO accessResourcesDTO);

    @Mappings({})
    List<Resource> formToDtoS(List<AccessResourcesDTO> accessResourcesDTO);
}
