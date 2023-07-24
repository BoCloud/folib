<template>
  <div>
    <div class="other-text" >其他登录方式</div>
    <div class="item-con-p">
    <div v-for="(item,index) in clients"
         :key="index" style="margin-top:20px">
         <a-tooltip>
          <template slot="title">
            {{ item.clientName }}
          </template>
      <div @click="toSingleLogin(item)" class="item-con">
       
          <div class="text-icon" :class="textBg(index)">{{ item.clientId.substring(0,1) }} </div>
        
      </div>   
    </a-tooltip>
    </div>
    </div>
  </div>

</template>

<script>
export default {
  props: {
    clients: {
      type: Array,
      default: []
    }
  },
  data() {
    return {
      textColor:[
        "text-secondary",
        "text-success",
        "text-gray-6",
        "text-warning"

      ]
    }
  },
  methods: {
    textBg(index){
     return this.textColor[index%4]
    },
    toSingleLogin(clientObject){ 
    let url =clientObject.ssoPath+"?redirect_uri="+clientObject.redirectPath+"&client_id="+clientObject.clientId+"&response_type=code"
    // 可以在输入的时候限定格式
    url= url.startsWith("http")? url:"http://"+url

    sessionStorage.setItem('loginMethod','single')
    // 这里要给退出url的地址
    sessionStorage.setItem('loginOutUti',clientObject.loginOutUrl+"?client_id="+clientObject.clientId+"&post_logout_redirect_uri="+clientObject.loginOutRedPath)
    sessionStorage.setItem('clientInfo',JSON.stringify(clientObject) )
    // 跳转到登陆页面
    window.location.href=url
    }
  }
}
</script>

<style lang="scss" scoped>

.other-text{
    font-size: 14px;
    font-weight: 700;
    color: #141414;

}

.text-icon{
  font-size: 20px;
  color: #fff;
  line-height: 25px;
  width: 25px;
  height: 25px;
  text-align: center;
  border-radius: 15px;;
}

.text-content{
  height: 30px;
  line-height: 30px;
  margin-left: 10px;
  font-weight: 500;
  color: #000;

}

.item-con-p{
  display: flex;
}
.item-con{
  display: flex;
   margin-left: 5px;
   padding: 10px 30px;
   border: 1px solid #bfbfbf;
   border-radius: 5px;
   text-align: center;
   .text-secondary{
    background: #1890FF;
    color: #fff !important;
  
  }
  .text-success{
    background: #52c418;
    color: #fff !important;
  
  }
  .text-gray-6{
    background: #f8f8f8;
    color: #fff!important;
  
  }
  .text-warning{
    background: #fadb14;
    color: #fff!important;
  
  }
}

.item-con:hover{
  cursor: pointer;
  border-color: #1890ff;
  
}
</style>
