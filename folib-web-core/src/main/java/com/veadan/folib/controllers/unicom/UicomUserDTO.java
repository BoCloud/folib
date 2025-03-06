package com.veadan.folib.controllers.unicom;

import com.veadan.folib.dto.UserDTO;
import lombok.Data;

import java.util.List;

/**
 * @author huayanjun
 * @since 2024-09-20 13:49
 */
@Data
public class UicomUserDTO {


    private String code;                               // 请求返回码
    private String message;                         // 请求消息
    private UicomUserDTO.UserDTO data;

    private String loginName;

    private String name;

    private String mobile;

    private String email;


    @Data
    public static class UserDTO{
        private String loginName;

        private String name;

        private String mobile;

        private String email;
    }

    public void fullFiled(){
        if(data != null){
            if(this.loginName == null){
                this.loginName = data.getLoginName();
            }
            if(this.name == null){
                this.name = data.getName();
            }
            if(this.mobile == null){
                this.mobile = data.getMobile();
            }
            if(this.email == null){
                this.email = data.getEmail();
            }
        }

    }


}
