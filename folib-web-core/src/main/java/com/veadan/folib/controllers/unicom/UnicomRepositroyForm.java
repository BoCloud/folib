package com.veadan.folib.controllers.unicom;

import com.veadan.folib.forms.configuration.RepositoryForm;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.Data;

import java.util.LinkedList;
import java.util.List;

/**
 * @author huayanjun
 * @since 2024-12-09 14:51
 */
@Data
public class UnicomRepositroyForm extends RepositoryForm {

    //项目名
    private String projectName;

    //登录名
    private String creator;

    private String email;

    // 子仓库
    List<SubRepo> subRepoList;

    @Data
    public static class SubRepo{
        private String id;
        private String policy;

    }

    public List<UnicomRepo> genRepoInfo(){
        List<UnicomRepo> repos = new LinkedList<>();
        UnicomRepo group = new UnicomRepo();
        group.setId(this.getId());
        group.setType(this.getType());
        group.setPolicy(this.getPolicy());
        repos.add(group);
        for (SubRepo subRepo : this.subRepoList) {
            UnicomRepo sub = new UnicomRepo();
            sub.setPolicy(subRepo.getPolicy());
            sub.setId(subRepo.getId());
            sub.setType(RepositoryTypeEnum.HOSTED.getType());
            repos.add(sub);
        }
        return repos;
    }

}
