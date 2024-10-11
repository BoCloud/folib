package com.veadan.folib.domain;

import com.veadan.folib.event.privilege.PrivilegeEventTypeEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotEmpty;

/**
 * 权限分发实体
 *
 * @author qijianping
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class PrivilegeDispatch {

    private PrivilegeEventTypeEnum privilegeEventTypeEnum;
//    @NotEmpty
    private String uuId;
    @NotEmpty
    private String targetHostName;

}
