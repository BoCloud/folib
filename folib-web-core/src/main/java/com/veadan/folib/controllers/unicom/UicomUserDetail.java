package com.veadan.folib.controllers.unicom;

import lombok.Data;

import java.util.List;

/**
 * @author huayanjun
 * @since 2024-09-23 14:00
 */
@Data
public class UicomUserDetail {
    private String id;
    // 登录名
    private String loginName;
    // 姓名
    private String name;
    //手机号码
    private String mobile;
    //用户邮箱
    private String email;
    //用户归属公司Id
    private String companyId;
    // 用户归属公司名
    private String companyName;
    // 用户归属部门Id
    private String officeId;
    // 用户归属部门名
    private String officeName;
    // 用户归属应用列表
    private List<UicomProject> projects;

    @Data
    public static class UicomProject{
        // 应用ID
        private String projectId;
        // 应用名称
        private String projectName;
        // 项目负责人
        private String projectManager;
        // 项目负责人邮箱
        private String projectManagerEmail;
        // 项目负责人手机号
        private String projectManagerMobile;
        // 用户在该项目内归属 '1':自有，'2':厂商
        private String isOwn;
        // 用户在该项目内职责
        private String duty;
    }
}
