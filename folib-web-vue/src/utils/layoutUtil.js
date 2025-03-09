export function getLayoutType(item) {
    const layout = item?.layout
    let layoutCast
    if (layout === "Maven 2") {
        layoutCast = item.subLayout ? item.subLayout : 'maven'
        return layoutCast
    }
    if (layout === "npm") {
        layoutCast = item.subLayout ? item.subLayout : 'npm'
        return layoutCast
    }
    if(layout === "Docker"){
      return  item.subLayout ? item.subLayout : 'docker'
    }
    return layout === "NuGet" ? "nuget" : layout === "Raw" ? "raw" : layout === "PyPi" ? "pypi" : layout ==="GitLfs" ? "gitlfs" : layout ==="HuggingFace" ? "huggingface" : layout === "Pub" ? "pub" : layout === "rpm" ? "rpm" : layout
}

export function getFileImage(layout, name) {
    if (layout.toLowerCase() === 'docker') {
        return "docker-s"
    }
    return getFileType(name)
}

export function getLayoutType2(layout, repository, type) {
    let layoutCast
    if (layout === "Maven 2" && type === "black") {
        layoutCast = repository.subLayout ? repository.subLayout : 'maven'
        return layoutCast
    }
    if (layout === "npm") {
        layoutCast = repository.subLayout ? repository.subLayout : 'npm'
        return layoutCast
    }
    if(layout === "Docker"){
        layoutCast = repository.subLayout ? repository.subLayout : 'docker'
        return layoutCast
    }
    return layout === "NuGet" ? "nuget" : layout === "Raw" ? "raw" : layout === "PyPi" ? "pypi" : layout === "Docker" ? "docker" : layout ==="GitLfs" ? "gitlfs" : layout ==="HuggingFace" ? "huggingface" : layout === "Pub" ? "pub" : layout === "rpm" ? "rpm" : layout
}

export function genLayoutType(layout) {
    layout = layout.toLowerCase()

    if (layout === "maven" || layout === "ivy" || layout === "sbt" || layout === "gradle") {
        return "Maven 2"
    }
    if (layout === "npm" || layout === "yarn" || layout === "ohpm") {

        return "npm"
    }
    if(layout === "docker" || layout === "ollama"){
        return "Docker"
    }
    return layout === "nuget" ? "NuGet" : layout === "raw" ? "Raw" : layout === "pypi" ? "PyPi" : layout === "rpm" ? "rpm" : layout ==="gitlfs" ? "GitLfs" : layout ==="huggingface" ? "HuggingFace" : layout === "Pub" ? "pub" : layout;
}

export function groupRepositoriesBuild(repositories) {
    let groupRepositories = []
    // console.log(repositories)
    if (repositories.length > 0) {
        repositories.forEach((item, index, repositories) => {
            groupRepositories.push(item.id)
        })
        return groupRepositories;
    }
    return groupRepositories;
}

export function objectToGroupRepositories(groupRepositoriesNameList, allRepositories, currentRepositoryId) {
    if (!groupRepositoriesNameList) {
        groupRepositoriesNameList = []
    }
    const aaa = allRepositories.filter(item => item.id !== currentRepositoryId)
    // console.log(aaa)
    let bordObjcet = {
        isSelect: aaa.filter(item => groupRepositoriesNameList.indexOf(item.id) > -1),
        enableSelect: aaa.filter(item => !(groupRepositoriesNameList.indexOf(item.id) > -1))
    }
    bordObjcet.isSelect.sort((a, b) => {
        return groupRepositoriesNameList.indexOf(a.id) - groupRepositoriesNameList.indexOf(b.id);
    });
    return bordObjcet;
}

export function getFileType(name) {

    return name.search(".jar") !== -1 ? "jar" : name.search(".xml") !== -1 || name.search(".pom") !== -1 ? "xml" : name.search(".zip") !== -1 || name.search(".rar") !== -1 || name.search(".gz") !== -1 || name.search(".tar") !== -1 ? "zip" : name.search(".properties") !== -1 || name.search(".yml") || name.search(".yaml") ? "yaml" : "unknown"
}

export function fileSizeConver(limit) {
    limit = Math.abs(limit)
    var size = "";
    if (limit === 0) {
        size = limit.toFixed(2) + "B";
    } else if (limit < 0.1 * 1024) {
        //如果小于0.1KB转化成B
        size = limit.toFixed(2) + "B";
    } else if (limit < 0.1 * 1024 * 1024) {
        //如果小于0.1MB转化成KB
        size = (limit / 1024).toFixed(2) + "KB";
    } else if (limit < 0.1 * 1024 * 1024 * 1024) {
        //如果小于0.1GB转化成MB
        size = (limit / (1024 * 1024)).toFixed(2) + "MB";
    } else {
        //其他转化成GB
        size = (limit / (1024 * 1024 * 1024)).toFixed(2) + "GB";
    }
    var sizestr = size + "";
    var len = sizestr.indexOf("\.");
    var dec = sizestr.substr(len + 1, 2);
    if (dec == "00") {
        //当小数点后为00时 去掉小数部分
        return sizestr.substring(0, len) + sizestr.substr(len + 3, 2);
    }
    return sizestr;
}

export function fileSizeConverUnit(limit, unit) {
    limit = Math.abs(limit)
    var size = "";
    switch (unit) {
        case "B":
            size = limit.toFixed(2);
            break;
        case "KB":
            size = (limit / 1024).toFixed(2);
            break;
        case "MB":
            size = (limit / (1024 * 1024)).toFixed(2);
            break;
        case "GB":
            size = (limit / (1024 * 1024 * 1024)).toFixed(2);
            break;
        default:
            size = (limit / (1024 * 1024 * 1024)).toFixed(2);
            break;
    }
    var sizestr = size + "";
    var len = sizestr.indexOf("\.");
    var dec = sizestr.substr(len + 1, 2);
    if (dec == "00") {
        //当小数点后为00时 去掉小数部分
        return sizestr.substring(0, len) + sizestr.substr(len + 3, 2);
    }
    return sizestr;
}

export function artifactCheck(repository, fileName, fileSize,maxSize) {
    let layout = repository.layout
    let result = {check: true, msg: ''}
    if (layout === 'Maven 2') {
        let policy = repository.policy
        let regex = /^(.*)-([0-9]{8}.[0-9]{6})-([0-9]+)(.*)$/
        let isSnapshot = fileName.indexOf('SNAPSHOT') !== -1 || regex.test(fileName)
        let msg = null
        if (policy === 'release' && isSnapshot) {
            msg = fileName + '为snapshot版本，仓库版本策略为release，禁止上传'
        }
        if (policy === 'snapshot' && !isSnapshot) {
        msg = fileName + '为release版本，仓库版本策略为snapshot，禁止上传'
        }
        if (msg) {
            result.check = false
            result.msg = msg
            return result
        }
    }
    let fileSizeLimit = maxSize ? maxSize :4 * 1024 * 1024 * 1024
    if (fileSize > fileSizeLimit) {
        let  mss= convertToUnit(fileSizeLimit);
        let msg = "文件大小超过"+mss+"上传限制";
        result.check = false
        result.msg = msg
        return result
    }
    return result
}

export function getLayoutRepoPrefix(item) {
    let prefix = 'artifactory/'
    let subLayout = item.subLayout
    switch(subLayout) {
        case 'pypi':
            prefix = 'artifactory/api/pypi/'
        break;
        case 'npm':
            prefix = 'artifactory/api/npm/'
        break;
        case 'ohpm':
            prefix = 'artifactory/api/ohpm/'
        break;
        case 'php':
            prefix = 'artifactory/api/composer/'
        break;
        case 'conan':
            prefix = 'artifactory/api/conan/'
        break;
        case 'helm':
            prefix = 'artifactory/api/helm/'
        break;
        case 'cocoapods':
            prefix = 'artifactory/api/pods/'
        break;
        case 'go':
            prefix = 'artifactory/api/go/'
        break;
        case 'gitlfs':
            prefix = 'artifactory/api/lfs/'
        break;
        case 'huggingface':
            prefix = 'artifactory/api/huggingfaceml/'
        break;
        case 'pub':
            prefix = 'artifactory/api/pub/'
        break;
        case 'docker':
            prefix = ''
        break;
        case 'ollama':
            prefix = ''
        break;
        case 'cargo':
            prefix = 'artifactory/api/cargo/'
        break;
        default:
            prefix = 'artifactory/'
    }
    return prefix
}

export function convertToUnit(size) {
    const sizeNumeric = Number(size);
    if (isNaN(sizeNumeric)) {
        throw new Error("Invalid size value: " + size);
    }
    if(size<1024){
        return sizeNumeric + "B";
    }else if(size>=1024 && size<1024*1024){
        return (sizeNumeric/1024).toFixed(2)+"KB";
    }else if(size>=1024*1024 && size<1024*1024*1024){
        return (sizeNumeric/(1024*1024)).toFixed(2)+"MB";
    }else if(size>=1024*1024*1024 && size<1024*1024*1024*1024){
        return (sizeNumeric/(1024*1024*1024)).toFixed(2)+"GB";
    }else if(size>=1024*1024*1024*1024 && size<1024*1024*1024*1024*1024){
        return (sizeNumeric/(1024*1024*1024*1024)).toFixed(2)+"TB";
    }else if(size>=1024*1024*1024*1024*1024 && size<1024*1024*1024*1024*1024*1024){
        return (sizeNumeric/(1024*1024*1024*1024*1024)).toFixed(2)+"PB";
    }else {
        throw new Error("Unsupported unit: " + unit);
    }

}

