package com.veadan.folib.ws.client.handler.command;

import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.domain.PromotionFileRelativePath;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.promotion.PullArtifactTask;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactPromotionService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.FutureTask;

import static com.veadan.folib.utils.UrlUtils.parsePath;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:37
 * @since x.x.x
 */
@Slf4j
@Component
public class FolibWsClientArtifactPullCommand implements FolibWsClientCommand<PromotionNodeOption> 
{
    public static final String COMMAND = "/client/artifact/pull";
    private static final String API_ARTIFACT_FOLIB_PROMOTION_GET_FILE_RELATIVE_PATHS = "/api/artifact/folib/promotion/getFileRelativePaths";
    
    @Autowired
    private ArtifactPromotionService artifactPromotionService;
    @Inject
    private ArtifactManagementService artifactManagementService;
    
    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Autowired
    private PromotionUtil promotionUtil;
    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;
    @Autowired
    private ThreadPoolTaskExecutor asyncRepositoryThreadPoolExecutor;
    @Inject
    private SecurityComponent securityComponent;
    
    
    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(PromotionNodeOption promotionNodeOption) 
    { 
        try 
        {
            String sourcePath = promotionNodeOption.getSourcePath();
            String targetPath = promotionNodeOption.getTargetPath();
            String srcStorageId = parsePath(sourcePath)[0];
            String srcRepostoryId = parsePath(sourcePath)[1];
            String srcUrl = sourcePath.split("/" + srcStorageId + "/" + srcRepostoryId + "/")[0];
            String srcUri = sourcePath.split("/" + srcStorageId + "/" + srcRepostoryId + "/")[1];
            String targetStorageId =  parsePath(targetPath)[0];
            String targetRepostoryId = parsePath(targetPath)[1];
            String targetUrl = targetPath.split("/" + targetStorageId + "/" + targetRepostoryId + "/")[0];
            String targetUri = targetPath.split("/" + targetStorageId + "/" + targetRepostoryId + "/")[1];

            log.info("进入拉模式={}",true);
            artifactPromotionService.validateStorageAndRepository(targetStorageId, targetRepostoryId);
            // 从源仓路径 pull 到目标仓路径 获取目标主机的path 路径下的文件与目录 然后依次提交到任务队列里面后将文件存入仓库
            String url = srcUrl + API_ARTIFACT_FOLIB_PROMOTION_GET_FILE_RELATIVE_PATHS;
            Client client = clientPool.getRestClient();
            WebTarget target = client.target(url);
            ArtifactDto artifactDto = ArtifactDto.builder().storageId(srcStorageId).
                    repostoryId(srcRepostoryId).path(srcUri).build();
            Invocation.Builder builder = target.request();
            securityComponent.securityTokenHeader(builder);
            Response response = builder.
                    post(Entity.entity(artifactDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() != 200) {
                throw new Exception("{} get error" + url);
            }
            PromotionFileRelativePath promotionFileRelativePath = response.readEntity(PromotionFileRelativePath.class);
            List<String> getFileRelativePaths = promotionFileRelativePath.getList();
            Map<String, Object> metaDataMap = promotionFileRelativePath.getMetaData();

            // 添加task
            List<FutureTask<String>> listTask = new ArrayList<>();
            for (String path : getFileRelativePaths) {
                ArtifactDto artifac = ArtifactDto.builder().storageId(srcStorageId)
                        .repostoryId(srcRepostoryId).path(path).build();
                String fileUlr = srcUrl + "/api/artifact/folib/promotion/download";
                String metaData = metaDataMap.getOrDefault(path, "") == null ?
                        "" : metaDataMap.getOrDefault(path, "").toString();
                PullArtifactTask pullArtifactTask = new PullArtifactTask(path, fileUlr, targetStorageId,
                        targetRepostoryId, repositoryPathResolver, artifactManagementService, clientPool,
                        promotionUtil, artifac, metaData);
                FutureTask<String> futureTask = new FutureTask<String>(pullArtifactTask);
                listTask.add(futureTask);
                asyncRepositoryThreadPoolExecutor.submit(futureTask);
            }
            int success = 0;
            int fail = 0;
            for (FutureTask<String> task : listTask) {
                try {
                    task.get();
                    success++;

                } catch (Exception e) {
                    fail++;
                    log.error("pull fail {}", e.getMessage());
                }
            }
            log.info("Handle pulled! Task size {} success {} fail {}", listTask.size(), success, fail);
            listTask.clear();
        }catch (Exception e)
        { log.error("拉取制品失败", e); }
    }

}
