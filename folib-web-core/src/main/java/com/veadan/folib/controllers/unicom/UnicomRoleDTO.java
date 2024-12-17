package com.veadan.folib.controllers.unicom;

import lombok.Data;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * 根据用户邮箱获取用户角色
 * @author huayanjun
 * @since 2024-12-03 10:01
 */
@Data
public class UnicomRoleDTO {
    private int code;                               // 请求返回码
    private String message;                         // 请求消息
    private List<UserData> data;                    // 用户数据列表

    @Data
    public static class UserData {
        private String userEmail;                     // 用户邮箱
        private List<ProjectInfo> info;               // 用户所拥有的项目和角色信息数组
    }

    @Data
    public static class ProjectInfo {
        private String projectName;                   // 归属应用名
        private String userId;                        // 用户id
        private String projectId;                     // 归属应用id
        private String email;                         // 用户邮箱
        private String loginName;                     // 用户登录名
        private List<RoleInfo> roles;                 // 用户在该应用下拥有的角色集合
    }

    @Data
    public static class RoleInfo {
        private String loginName;                     // 用户登录名
        private String name;                          // 用户角色
        private String id;                            // 角色id
        private String projectId;                     // 归属应用id
        private String email;                         // 用户邮箱
    }

    public Set<String> ownProject(){
        List<ProjectInfo> info = this.data.get(0).getInfo();
        HashSet<String> projects=new HashSet<>();
        for (ProjectInfo projectInfo : info) {
            for (RoleInfo role : projectInfo.getRoles()) {
                if(UnicomAdapter.adminRole.contains(role.name)){
                    projects.add(projectInfo.getProjectId());
                    break;
                }
            }
        }
        return projects;
    }
}
