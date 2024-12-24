package com.veadan.folib.controllers.federal.req;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class FederalRepositoryBaseReq {

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
     * 节点类型[inner:内部节点,JFrog:外部节点]
     */
    private String nodeType;
}
