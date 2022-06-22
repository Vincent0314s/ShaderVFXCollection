using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SkillTester : MonoBehaviour
{
    public VFX_BaseSkill skillToTest;
    [SerializeField]
    private Transform skillPoint;
    private Animator anim;
    private bool isCastingSkill;

    void Awake(){
        anim = transform.GetChild(0).GetComponent<Animator>();

    }
    void Start()
    {
        
    }

    void Update()
    {
        if(Input.GetKeyDown(KeyCode.Space)){
           CreateSkillVFX();
        }

        anim.SetBool("isCasting",isCastingSkill);
    }
    
    void CreateSkillVFX(){
        isCastingSkill = true;
        VFX_BaseSkill currentVFX = Instantiate(skillToTest,skillPoint.position,Quaternion.identity);
        currentVFX.LaunchSkill(() => isCastingSkill = false);
    }
}
