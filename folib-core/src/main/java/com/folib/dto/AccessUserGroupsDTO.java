package com.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * @author veadan
 * @Date: 2024/8/2 10:24
 * @Description:
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccessUserGroupsDTO {
    /**用户组id*/
    private String id;
    /**用户组名称*/
    private String name;

    private List<String> access = new ArrayList<>();
}
