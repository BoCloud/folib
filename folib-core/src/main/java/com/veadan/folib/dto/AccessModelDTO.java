package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AccessModelDTO
{
    private List<AccessUserGroupsDTO> groups;
    private List<AccessUsersDTO> users;

}
