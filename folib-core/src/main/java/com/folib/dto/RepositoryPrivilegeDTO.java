package com.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @Date: 2024/8/12 15:46
 * @Description:
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RepositoryPrivilegeDTO {

   private String storage;
   private String repository;
   private String privilege;
}
