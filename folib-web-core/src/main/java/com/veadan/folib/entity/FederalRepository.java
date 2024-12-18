package com.veadan.folib.entity;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;


/**
 * 联邦仓库表;
 *
 * @author : pj
 * @date : 2024-11-21
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name ="federal_repository")
public class FederalRepository implements Serializable {
    /**
     * id
     */
    @Id
    private long id;
    /**
     * 策略ID
     */
    @Column(name = "policy_id")
    private long policyId;
    /**
     * 联邦库类型:[source，target]
     */
    private String type;
    /**
     * 存储空间ID
     */
    @Column(name = "storage_id")
    private String storageId;
    /**
     * 仓库ID
     */
    @Column(name = "repository_id")
    private String repositoryId;
    /**
     * 节点名称：目标库才有的属性
     */
    @Column(name = "node_name")
    private String nodeName;
    /**
     * 节点类型[inner:内部节点,external:外部节点]
     */
    @Column(name = "node_type")
    private String nodeType;
    /**
     * 创建时间
     */
    @Column(name = "create_time")
    private Date createdTime;
    /**
     * 更新时间
     */
    @Column(name = "update_time")
    private Date updateTime;

}
