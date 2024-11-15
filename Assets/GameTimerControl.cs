using UnityEngine;

public class GameTimerControl : MonoBehaviour
{
    public bool resetScene = true;
    public float TimeUntilCycleReset = 120.0f;
    private bool isTimeUp = false;
    public float TimeBetweenCycleResetandSceneReset = 15.0f;
    private float CycleTime;
    private float PostCycleTime;
    public GameObject playerReference;
    private Vector3 playerStartPosition;
    private Quaternion playerStartRotation;

    public bool pressPToEndGameDebug = false;
    private bool fadeStarted = false;

    // Start is called once before the first execution of Update after the MonoBehaviour is created
    void Start()
    {
        CycleTime = TimeUntilCycleReset;
        PostCycleTime = TimeBetweenCycleResetandSceneReset;
    }

    private void Awake()
    {
        playerStartPosition = playerReference.transform.position;
        playerStartRotation = playerReference.transform.rotation;

    }

    // Update is called once per frame
    void Update()
    {
        CycleTime -= Time.deltaTime;

        if(CycleTime <= 0)
        {
            isTimeUp = true;
        }

        if(isTimeUp)
        {
            PostCycleTime -= Time.deltaTime;
            if(PostCycleTime <= 1 && fadeStarted == false)
            {
                fadeStarted = true;
                GameObject.Find("MainCamera").GetComponent<FadeCamera>().fadeToBlack = true;
                GameObject.Find("MainCamera").GetComponent<FadeCamera>().RedoFade();

            }

            if (PostCycleTime <= 0)
            {
                ResetWorld();
            }
        }

        if(pressPToEndGameDebug)
        {
            if(Input.GetKey(KeyCode.P))
            {
                ResetWorld();
            }
        }


    }

    public void ResetWorld()
    {
        Debug.Log("Time Reset");
        if(playerReference != null)
        {
            Debug.Log("Resetting position");
            CharacterController _controller = playerReference.GetComponent<CharacterController>();
            _controller.enabled = false;
            playerReference.transform.position = playerStartPosition;
            playerReference.transform.rotation = playerStartRotation;
            playerReference.GetComponent<CharacterController>().enabled = true;
        }
        

        CycleTime = TimeUntilCycleReset;
        PostCycleTime = TimeBetweenCycleResetandSceneReset;
    }


}
