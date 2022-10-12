package com.veadan.folib.services;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author leipenghui
 * @date 2022/10/8
 **/
public interface ArtifactWebService {

    /**
     * 导出受漏洞影响的制品信息
     *
     * @param vulnerabilityUuid 漏洞id
     * @param storageId         存储空间id
     * @param repositoryId      仓库id
     * @param response          响应流
     * @throws IOException 异常
     */
    void exportExcel(String vulnerabilityUuid,
                     String storageId,
                     String repositoryId, HttpServletResponse response) throws IOException;

    /**
     * 导出受漏洞影响的制品信息
     *
     * @param vulnerabilityUuid 漏洞id
     * @param storageId         存储空间id
     * @param repositoryId      仓库id
     */
    void exportPdf(String vulnerabilityUuid,
                   String storageId,
                   String repositoryId);
}
