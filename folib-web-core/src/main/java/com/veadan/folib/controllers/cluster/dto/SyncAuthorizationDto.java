package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.cluster.SyncAuthorizationEnum;
import com.veadan.folib.cluster.SyncMetadataEnum;
import com.veadan.folib.configuration.MutableMetadataConfiguration;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 *
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SyncAuthorizationDto {

    private AuthorizationConfigDto authorizationConfigDto;

    private SyncAuthorizationEnum syncAuthorizationEnum;
}
