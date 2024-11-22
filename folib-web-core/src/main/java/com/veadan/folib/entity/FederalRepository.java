package com.veadan.folib.entity;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

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
public class FederalRepository implements Serializable, Cloneable {
    /**
     * id
     */
    @Id
    private long id;
    /**
     * 策略ID
     */
    private long policyId;
    /**
     * 联邦库类型:[source，target]
     */
    private String type;
    /**
     * 存储空间ID
     */
    private String storageId;
    /**
     * 仓库ID
     */
    private String repositoryId;
    /**
     * 节点名称：目标库才有的属性
     */
    private String nodeName;
    /**
     * 节点类型[inner:内部节点,external:外部节点]
     */
    private String nodeType;
    /**
     * 创建时间
     */
    private Date createdTime;
    /**
     * 更新时间
     */
    private Date updateTime;

}
