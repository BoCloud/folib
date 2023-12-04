<template>
  <div v-if="clients && clients.length >0">
    <div class="other-text" >其他登录方式</div>
    <div class="item-con-p">
    <div v-for="(item,index) in clients"
         :key="index" style="margin-top:20px">
         <a-tooltip>
          <template slot="title">
            {{ item.clientName }}
          </template>
      <div @click="toSingleLogin(item)" class="item-con">
       
          <div class="text-icon" :class="textBg(index)">{{ item.clientName.substring(0,1) }} </div>
        
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
    toSingleLogin(client){ 
    let url =client.ssoPath+"?redirect_uri="+client.redirectPath+"&client_id="+client.clientId+"&state="+client.clientId+"&response_type=code&scope=openid%20profile%20email"
    // 可以在输入的时候限定格式
    url= url.startsWith("http")? url:"http://"+url
    // 缓存SSO退出地址
    localStorage.setItem('SSOLogout',client.loginOutUrl+"?client_id="+client.clientId+"&post_logout_redirect_uri="+client.loginOutRedPath)
    // 跳转到SSO登录页面
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
