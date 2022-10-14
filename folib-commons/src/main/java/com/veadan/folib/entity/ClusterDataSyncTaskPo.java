package com.veadan.folib.entity;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.math.BigInteger;

@Data
@Table(name = "cluster_datasync_task")
@ApiModel("cluster_datasync_task")
public class ClusterDataSyncTaskPo {
    @Id
    @ApiModelProperty("uuid 主键id")
    @Column(name = "id")
    private String id;

    @ApiModelProperty("节点ip")
    @Column(name = "host")
    private String host;

    @ApiModelProperty("待同步的数据")
    @Column(name = "data_json")
    private String dataJson;

    @ApiModelProperty("任务类型")
    @Column(name = "task_type")
    private Integer taskType;

    @ApiModelProperty("任务状态")
    @Column(name = "status")
    private Integer status;

    @ApiModelProperty("请求的url")
    @Column(name = "url")
    private String url;

    @ApiModelProperty("当前时间")
    @Column(name = "current_time_millis")
    private BigInteger currentTimeMillis;

    public ClusterDataSyncTaskPo() {
    }

    public ClusterDataSyncTaskPo(String id, String host, String dataJson, Integer taskType, Integer status, String url, BigInteger currentTimeMillis) {
        this.id = id;
        this.host = host;
        this.dataJson = dataJson;
        this.taskType = taskType;
        this.status = status;
        this.url = url;
        this.currentTimeMillis = currentTimeMillis;
    }
}
