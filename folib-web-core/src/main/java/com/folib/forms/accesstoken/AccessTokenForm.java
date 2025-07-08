package com.folib.forms.accesstoken;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

/**
 * @author huayanjun
 * @since 2024-08-20 14:11
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AccessTokenForm {

    private String description;


    //0-不过期 1-7天 2-30天 3-90天 4-1年
    @NotNull(message = "过期时间不能为空")
    private Integer expire;

    @NotBlank(message = "用户不能为空")
    private String username;


}
