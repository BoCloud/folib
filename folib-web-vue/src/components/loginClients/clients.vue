<template>
  <div>
    <div class="other-text" >其他登录方式</div>
    <div class="item-con-p">
    <div v-for="(item,index) in clients"
         :key="index" style="margin-top:20px">
      <div @click="toSingleLogin(item)" class="item-con">
        <div class="text-icon">{{ item.clientId.substring(0,1) }} </div>
        <div class="text-content"> {{ item.clientName }}</div>
      </div>   
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
    return {}
  },
  methods: {
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
  background: #1890FF;
  line-height: 30px;
  width: 30px;
  height: 30px;
  text-align: center;
  border-radius: 15px;;
}

.text-content{
  height: 30px;
  line-height: 30px;
  margin-left: 5px;
  font-weight: 500;
  color: #000;

}

.item-con-p{
  display: flex;
}
.item-con{
  display: flex;
   min-width: 100px;
   margin-left: 5px;
   padding: 5px;
   border: 1px solid #fff;
}

.item-con:hover{
  border: 1px solid #fff;
  cursor: pointer;
  border-bottom: 1px solid #1890FF;
  
}
</style>
