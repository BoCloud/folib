export function getLayoutType (item) {
    const layout=item.layout
    const id=item.id.toLowerCase()
    let layoutCast
    if(layout==="Maven 2"){
        layoutCast = id.search("ivy")!== -1?"ivy":id.search("sbt")!== -1?"sbt":id.search("gradle")!== -1?"gradle":"maven"
        return layoutCast
    }
    if(layout==="npm"){
        layoutCast = id.search("yarn")!== -1?"yarn":"npm"
        return layoutCast
    }
    return layout==="NuGet"?"nuget":layout==="Raw"?"raw":layout==="PyPi"?"pypi":layout==="Docker"?"docker":layout==="rpm"?"rpm":layout
}

export function getLayoutType2 (layout,repository,type) {

    const id=repository.toLowerCase()
    let layoutCast
    if(layout==="Maven 2"&&type==="black"){
        layoutCast = id.search("ivy")!== -1?"ivy":id.search("sbt")!== -1?"sbt":id.search("gradle")!== -1?"gradle":"maven_black"
        return layoutCast
    }
    if(layout==="npm"){
        layoutCast = id.search("yarn")!== -1?"yarn":"npm"
        return layoutCast
    }
    return layout==="NuGet"?"nuget":layout==="Raw"?"raw":layout==="PyPi"?"pypi":layout==="Docker"?"docker":layout==="rpm"?"rpm":layout
}

export function genLayoutType (layout) {
   layout =layout.toLowerCase()

    if(layout==="maven"||layout==="ivy"||layout==="sbt"||layout==="gradle"){
        return "Maven 2"
    }
    if(layout==="npm"||layout==="yarn"){

        return "npm"
    }
  return layout==="nuget"?"NuGet": layout==="raw"?"Raw":layout==="pypi"?"PyPi":layout==="docker"?"Docker":layout==="rpm"?"rpm":layout;

}

export function groupRepositoriesBuild (repositories) {
   let groupRepositories=[]
    // console.log(repositories)
    if(repositories.length>0){
        repositories.forEach((item,index,repositories)=>{
            groupRepositories.push(item.id)
        })
        return groupRepositories;
    }
    return groupRepositories;
}

export function objectToGroupRepositories (groupRepositoriesNameList,allRepositories,currentRepositoryId) {
    const aaa= allRepositories.filter(item => item.id!==currentRepositoryId)
    // console.log(aaa)
    let bordObjcet={
        isSelect: aaa.filter(item => groupRepositoriesNameList.indexOf(item.id)>-1),
        enableSelect: aaa.filter(item => !(groupRepositoriesNameList.indexOf(item.id)>-1))
    }
    bordObjcet.isSelect.sort((a,b)=>{
        return groupRepositoriesNameList.indexOf(a.id)- groupRepositoriesNameList.indexOf(b.id);
    });
    return bordObjcet;
}

export function getFileType (name) {

    return name.search(".jar")!== -1?"jar":name.search(".xml")!== -1||name.search(".pom")!== -1?"xml":name.search(".zip")!== -1||name.search(".rar")!== -1||name.search(".gz")!== -1||name.search(".tar")!== -1?"zip":name.search(".properties")!== -1||name.search(".yml")||name.search(".yaml")?"yaml":"unknown"
}

export function fileSizeConver(limit){
    var size = "";
    if( limit < 0.1 * 1024 ){
        //如果小于0.1KB转化成B
        size = limit.toFixed(2) + "B";
    }else if(limit < 0.1 * 1024 * 1024 ){
        //如果小于0.1MB转化成KB
        size = (limit / 1024).toFixed(2) + "KB";
    }else if(limit < 0.1 * 1024 * 1024 * 1024){
        //如果小于0.1GB转化成MB
        size = (limit / (1024 * 1024)).toFixed(2) + "MB";
    }else{
        //其他转化成GB
        size = (limit / (1024 * 1024 * 1024)).toFixed(2) + "GB";
    }
    var sizestr = size + "";
    var len = sizestr.indexOf("\.");
    var dec = sizestr.substr(len + 1, 2);
    if(dec == "00"){
        //当小数点后为00时 去掉小数部分
        return sizestr.substring(0,len) + sizestr.substr(len + 3,2);
    }
    return sizestr;
}



