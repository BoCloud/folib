package com.folib.entity;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;
import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.sql.Timestamp;

@Data
@Accessors(chain = true)
@Table(name = "folib_scanner")
@ApiModel("folib_scanner")
public class FolibLock implements Serializable {

    @Id
    @ApiModelProperty("锁的路径名称")
    @Column(name = "name")
    private String name;

    @ApiModelProperty("锁定直到")
    @Column(name = "lock_until")
    private Timestamp lockUntil;

    @ApiModelProperty("锁定的时间")
    @Column(name = "lock_at")
    private Timestamp lockedAt;

    @ApiModelProperty("锁定者")
    @Column(name = "locked_by")
    private String lockedBy;


}
