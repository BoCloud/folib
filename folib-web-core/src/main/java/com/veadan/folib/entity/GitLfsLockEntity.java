package com.veadan.folib.entity;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.io.Serializable;


@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "git_lfs_locks")
@ApiModel("GitLfsLock")
public class GitLfsLockEntity implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @Id
    @ApiModelProperty(name = "id", notes = "")
    private String id;
    /**
     * 存储ID
     */
    @ApiModelProperty(name = "存储ID", notes = "")
    private String storageId;
    /**
     * 仓库ID
     */
    @ApiModelProperty(name = "仓库ID", notes = "")
    private String repositoryId;
    /**
     * 锁定文件的路径
     */
    @ApiModelProperty(name = "锁定文件的路径", notes = "")
    private String path;
    /**
     * 创建锁的时间戳
     */
    @ApiModelProperty(name = "创建锁的时间戳", notes = "")
    private Long lockedAt;
    /**
     * 所属人
     */
    @ApiModelProperty(name = "所属人", notes = "")
    private String owner;
    /**
     * 描述锁所属的服务器引用
     */
    @ApiModelProperty(name = "描述锁所属的服务器引用", notes = "")
    private String ref;

}
